package Game::Server::Role::Movements;

use My::Moose::Role;
use Game::Config;
use Game::Object::Movement;
use Game::Mechanics::Movement qw(move_actor);
use Game::Checks::Map qw(can_move_to);

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
	isa => HashRef [InstanceOf ['Unit::Actor']],
	default => sub { {} },
);

sub set_movement_check ($self, $actor_id, $x, $y)
{
	my $actor = $self->location->get_actor($actor_id);
	can_move_to($actor, $self->map, $x, $y);

	$self->set_movement($actor, $x, $y);
	return;
}

sub set_movement ($self, $actor, $x, $y)
{
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

	my $actor_id = $actor->id;
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

	return false unless $movement;
	return move_actor($movement, $self->map);
}

sub _process_movements ($self)
{
	my $map = $self->map;
	my $elapsed = server_time;

	foreach my $actor (values $self->_moving->%*) {
		my $movement = $actor->stats->movement;

		if (!($movement && move_actor($movement, $map, $elapsed))) {
			delete $self->_moving->{$actor->id};

			if ($movement) {
				$actor->stats->clear_movement;

				$self->send_to_players(
					[$actor->id, $self->get_discovered_by($actor->id)],
					Resource::ActorPosition->new(subject => $actor)
				) unless $movement->finished;

				$self->signal(movement_ended => $actor);
			}
		}
	}

	return;
}

sub _cleanup_movement ($self, $actor)
{
	delete $self->_moving->{$actor->id};
}

sub _send_ongoing_movement ($self, $for_actor, $actor)
{
	$self->send_to_player($for_actor->id, Resource::ActorMovement->new(subject => $actor));
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.1 => '_process_movements', 10);
	$self->_add_signal(player_left => '_cleanup_movement');
	$self->_add_signal(actor_appeared => '_send_ongoing_movement', '$actor->stats->movement');
};

