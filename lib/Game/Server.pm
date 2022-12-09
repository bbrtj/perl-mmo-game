package Game::Server;

use My::Moose;
use Algorithm::QuadTree;
use Game::Config;
use Sub::Quote qw(quote_sub quotify);
use Time::HiRes qw(time);

use header;

has param 'process' => (
	isa => Types::InstanceOf ['Server::Process::Game'],
	weak_ref => 1,
	'handles->' => {
		'send_to_player' => 'send_to_player',
		'log' => 'log',
	},
);

has param 'location' => (
	isa => Types::InstanceOf ['Unit::Location'],
	'handles->' => {
		'get_player' => 'get_player',
	},
);

has field 'map' => (
	lazy => sub ($self) {
		my $location = $self->location->lore;
		croak 'no map for location ' . $location->id
			unless $location->data->has_map;

		return $location->data->map;
	}
);

has param 'start_time' => (
	isa => Types::PositiveNum,
	default => sub { time },
	'handles++' => {
		get_time => sub { time - shift },
	},
);

has field '_actions' => (
	isa => Types::HashRef [Types::ArrayRef],
	default => sub { {} },
);

has cached '_compiled_action' => (
	isa => Types::CodeRef,
	lazy => 1,
);

with qw(
	Game::Server::Role::QuadTree
	Game::Server::Role::Discovery
	Game::Server::Role::Movements
);

sub _add_action ($self, $every, $handler, $priority = 'normal')
{
	croak "$handler is not a proper method name in " . __PACKAGE__
		unless $self->can($handler);

	if ($priority eq 'high') {
		unshift $self->_actions->{$every}->@*, $handler;
	}
	else {
		push $self->_actions->{$every}->@*, $handler;
	}

	return;
}

sub _build_compiled_action ($self)
{
	my %actions = $self->_actions->%*;
	my @sorted =
		map { $_ => $actions{$_} }
		sort { $a <=> $b }
		keys %actions;

	my @actions_lines = (q[my ($elapsed) = @_;]);

	foreach my ($every, $handlers) (@sorted) {
		$every = quotify $every;
		push @actions_lines,
			qq[if (\$elapsed % $every == 0) {],
			(map { qq[ \$self->$_();] } $handlers->@*),
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

sub tick ($self, $elapsed)
{
	$self->_compiled_action->($elapsed);
}

