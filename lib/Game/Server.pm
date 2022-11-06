package Game::Server;

use My::Moose;
use Algorithm::QuadTree;
use Game::Config;
use Sub::Quote qw(quote_sub quotify);

use header;

has param 'process' => (
	isa => Types::InstanceOf ['Server::Process::Game'],
	weak_ref => 1,
	'handles->' => {
		'send_to' => 'send_to',
		'log' => 'log',
	},
);

has param 'location_data' => (
	isa => Types::InstanceOf ['Unit::Location'],
);

has field '_actions' => (
	isa => Types::HashRef [Types::ArrayRef],
	default => sub { {} },
);

has cache '_compiled_action' => (
	isa => Types::CodeRef,
	lazy => 1,
);

with qw(
	Game::Server::Role::QuadTree
	Game::Server::Role::Discovery
);

sub _add_action ($self, $every, $handler)
{
	croak "$handler is not a proper method name in " . __PACKAGE__
		unless $self->can($handler);

	push $self->_actions->{$every}->@*, $handler;
	return;
}

sub _build_compiled_action ($self)
{
	my %actions = $self->_actions->%*;
	my @sorted =
		map { $_ => $actions{$_} }
		sort { $b <=> $a }
		keys %actions;

	my @actions_lines = (q[my ($elapsed, $elapsed_time) = @_;]);

	foreach my ($every, $handlers) (@sorted){
		$every = quotify $every;
		push @actions_lines,
		qq[if (\$elapsed % $every == 0) {],
		(map { qq[ \$self->$_(\$elapsed_time);] } $handlers->@*),
		qq[}];
		}

		my $compiled = join "\n", @actions_lines;

	$self->log->debug("Compiled action: \n$compiled");

	return quote_sub $compiled, {
		'$self' => \$self,
		}, {
			no_defer => 1,
		};
}

sub BUILD ($self, $args)
{
	# no BUILD actions by default. See roles for "after" hooks
}

sub tick ($self, $elapsed, $elapsed_time)
{
	$self->_compiled_action->($elapsed, $elapsed_time);
}

