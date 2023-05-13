package Game::Server::Role::Movements;

use My::Moose::Role;
use Game::Config;
use Game::Object::Movement;
use Game::Mechanics::Movement;

use all 'X';
use all 'Resource';

use header;

requires qw(
	location
	send_to_player
	send_to_players
	get_discovered_by
);

has cached '_moving' => (
	isa => Types::HashRef [Types::InstanceOf ['Unit::Actor']],
	default => sub { {} },
);

sub set_movement ($self, $actor_id, $x, $y)
{
	my $actor = $self->location->get_actor($actor_id);
	$self->_process_movement($actor);

	$actor->stats->set_movement(
		Game::Object::Movement->new(
			variables => $actor->variables,
			x => $x,
			y => $y,
			speed => $actor->stats->speed,
			time => server_time,
		)
	);

	$self->_moving->{$actor_id} = $actor;

	$self->send_to_players(
		[$actor_id, $self->get_discovered_by($actor_id)],
		Resource::ActorMovement->new(subject => $actor)
	);

	return;
}

sub cancel_movement ($self, $actor_id)
{
	return unless exists $self->_moving->{$actor_id};
	my $actor = delete $self->_moving->{$actor_id};

	$self->_process_movement($actor);
	$actor->stats->clear_movement;

	$self->send_to_players(
		[$actor_id, $self->get_discovered_by($actor_id)],
		Resource::ActorPosition->new(subject => $actor)
	);

	return;
}

sub _process_movement ($self, $actor)
{
	my $movement = $actor->stats->movement;

	return !!0 unless $movement;
	return Game::Mechanics::Movement->move($movement, $self->map);
}

sub _process_movements ($self)
{
	my $map = $self->map;
	my $elapsed = server_time;

	foreach my $actor (values $self->_moving->%*) {
		my $movement = $actor->stats->movement;

		if (!($movement && Game::Mechanics::Movement->move($movement, $map, $elapsed))) {
			delete $self->_moving->{$actor->id};

			if ($movement) {
				$actor->stats->clear_movement;

				$self->send_to_players(
					[$actor->id, $self->get_discovered_by($actor->id)],
					Resource::ActorPosition->new(subject => $actor)
				) unless $movement->finished;
			}
		}
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_process_movements', 'high');
};

after signal_player_left => sub ($self, $actor) {
	delete $self->_moving->{$actor->id};
};

after signal_actor_appeared => sub ($self, $for_actor, $actor) {
	return unless $actor->stats->movement;

	$self->send_to_player($for_actor->id, Resource::ActorMovement->new(subject => $actor));
};

