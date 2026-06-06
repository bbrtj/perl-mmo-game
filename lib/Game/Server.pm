package Game::Server;

use My::Moose;
use Game::Config;
use Server::Config;
use Sub::Quote qw(quote_sub);
use POSIX qw(round);

use header;

has injected 'env';
has injected 'cache_repo';
has injected 'lore_data_repo';

has param 'process' => (
	isa => InstanceOf ['Server::Process::Game'],
	weak_ref => 1,
	'handles->' => {
		'send_to_player' => 'send_to_player',
		'send_to_players' => 'send_to_players',
		'log' => 'log',
	},
);

has param 'location' => (
	isa => InstanceOf ['Unit::Location'],
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

has field '_actions' => (
	isa => ArrayRef [HashRef [ArrayRef]],
	default => sub { [] },
);

has cached '_compiled_action' => (
	isa => CodeRef,
	lazy => 1,
);

has field '_queue' => (
	isa => ArrayRef [ArrayRef],
	default => sub { [] },
	'handles[]' => {
		'_clear_queue' => 'clear',
	},
);

with qw(
	Game::Server::Role::QuadTree
	Game::Server::Role::ActionQueue
	Game::Server::Role::Discovery

	Game::Server::Role::Chat
	Game::Server::Role::Projectiles
	Game::Server::Role::Combat
	Game::Server::Role::Movements
	Game::Server::Role::Regeneration
);

sub _add_action ($self, $every, $handler, $priority = 0)
{
	croak "$handler is not a proper method name in " . __PACKAGE__
		unless $self->can($handler);

	# $every is time - turn it into a server tick count
	$every = round($every / Server::Config::TICK);
	$every ||= 1;

	push $self->_actions->[$priority]->{$every}->@*, $handler;
	return;
}

sub _build_compiled_action ($self)
{
	my @actions_lines = (q[my ($elapsed) = @_;]);

	# high priority means actions come sooner
	for my $actions (reverse $self->_actions->@*) {
		next unless defined $actions;

		my @sorted =
			map { $_ => $actions->{$_} }
			sort { $a <=> $b }
			keys $actions->%*;

		foreach my ($every, $handlers) (@sorted) {
			push @actions_lines,
				qq[if (\$elapsed % $every == 0) {],
				(map { qq[ \$self->$_();] } $handlers->@*),
				qq[}];
		}
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

	return;
}

sub queue ($self, @data)
{
	push $self->_queue->@*, \@data;

	return;
}

sub resolve_queue ($self)
{
	state $method_map = {};

	foreach my $item ($self->_queue->@*) {
		my ($name, @args) = $item->@*;
		my $method = $method_map->{$name} // $self->can($name);

		$self->$method(@args);
	}

	$self->_clear_queue;

	return;
}

sub apply_effect ($self, $effect, @args)
{
	my $method = $effect->server_method;
	return $self->$method($effect, @args);
}

sub signal_player_left ($self, $actor)
{
	$self->location->remove_actor($actor);

	return;
}

sub signal_actor_appeared ($self, $for_actor, $actor)
{
	# $self->log->debug(sprintf "actor %s appeared for %s", $actor->id, $for_actor->id);

	return;
}

sub signal_projectile_appeared ($self, $for_actor, $projectile)
{
	# $self->log->debug(sprintf "projectile %s appeared for %s", $projectile->id, $for_actor->id);

	return;
}

