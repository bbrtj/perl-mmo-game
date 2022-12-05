package Game::Server::Role::Movements;

use My::Moose::Role;
use Game::Config;
use Game::Object::Movement;
use Game::Mechanics::Movement;

use header;

requires qw(
	location
	find_in_radius
	send_to_player
);

has cached '_movements' => (
	isa => Types::HashRef [Types::InstanceOf ['Game::Object::Movement']],
	default => sub { {} },
);

sub set_movement ($self, $actor_id, $x, $y)
{
	X::InvalidCoordinate->throw(msg => "$x;$y")
		unless $self->map->check_can_be_accessed($x, $y);

	my $variables = $self->location->get_actor($actor_id)->variables;
	my $speed = Game::Config->config->{base_speed}; # TODO

	my $movement = Game::Object::Movement->new(
		variables => $variables,
		x => $x,
		y => $y,
		speed => $speed,
		time => $self->get_time,
	);

	$self->_process_movement(delete $self->_movements->{$actor_id});
	$self->_movements->{$actor_id} = $movement;

	# TODO: notify other players in range
	return $movement;
}

sub cancel_movement ($self, $actor_id)
{
	my $movement = delete $self->_movements->{$actor_id};
	$self->_process_movement($movement);

	# TODO: notify other players in range
	return $movement;
}

sub _process_movement ($self, $movement, $elapsed = $self->get_time, $map = $self->map)
{
	return !!1 unless $movement;
	return Game::Mechanics::Movement->move($movement, $elapsed, $map);
}

sub _process_movements ($self)
{
	my $elapsed = $self->get_time;
	my $map = $self->map;

	foreach my ($actor_id, $movement) ($self->_movements->%*) {
		if (!$self->_process_movement($movement, $elapsed, $map)) {
			delete $self->_movements->{$actor_id};
			# TODO: notify the client to stop moving
		}
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_process_movements');
};

