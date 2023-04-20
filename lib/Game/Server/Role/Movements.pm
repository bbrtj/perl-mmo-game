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
	get_discovered_by
);

has cached '_movements' => (
	isa => Types::HashRef [Types::InstanceOf ['Game::Object::Movement']],
	default => sub { {} },
);

sub set_movement ($self, $actor_id, $x, $y)
{
	$self->_process_movement(delete $self->_movements->{$actor_id});

	my $actor = $self->location->get_actor($actor_id);
	my $speed = Game::Config->config->{base_speed};    # TODO

	my $movement = Game::Object::Movement->new(
		variables => $actor->variables,
		x => $x,
		y => $y,
		speed => $speed,
		time => $self->get_time,
	);

	$self->_movements->{$actor_id} = $movement;

	my $resource = Resource::ActorMovement->new(
		subject => $actor,
		movement => $movement
	);

	foreach my $id ($actor_id, $self->get_discovered_by($actor_id)) {
		$self->send_to_player($id, $resource);
	}

	return;
}

sub cancel_movement ($self, $actor_id)
{
	if (exists $self->_movements->{$actor_id}) {
		$self->_process_movement(delete $self->_movements->{$actor_id});

		my $resource = Resource::ActorMovementStopped->new(
			subject => $self->location->get_actor($actor_id),
		);

		foreach my $id ($actor_id, $self->get_discovered_by($actor_id)) {
			$self->send_to_player($id, $resource);
		}
	}

	return;
}

sub _process_movement ($self, $movement, $elapsed = $self->get_time, $map = $self->map)
{
	return !!0 unless $movement;
	return Game::Mechanics::Movement->move($movement, $elapsed, $map);
}

sub _process_movements ($self)
{
	my $elapsed = $self->get_time;
	my $map = $self->map;

	foreach my ($actor_id, $movement) ($self->_movements->%*) {
		if (!$self->_process_movement($movement, $elapsed, $map)) {
			delete $self->_movements->{$actor_id};

			if (!$movement->finished) {
				my $resource = Resource::ActorMovementStopped->new(
					subject => $self->location->get_actor($actor_id),
				);

				foreach my $id ($actor_id, $self->get_discovered_by($actor_id)) {
					$self->send_to_player($id, $resource);
				}
			}
		}
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_process_movements', 'high');
};

