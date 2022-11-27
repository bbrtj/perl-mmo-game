package Game::Server::Role::Movements;

use My::Moose::Role;
use Game::Config;
use Game::Server::Movement;
use Game::Mechanics::Movement;

use header;

requires qw(
	location_data
	find_in_radius
	send_to_actor
);

has cached '_movements' => (
	isa => Types::HashRef [Types::InstanceOf ['Game::Server::Movement']],
	default => sub { {} },
);

sub set_movement ($self, $actor, $x, $y)
{
	$self->cancel_movement($actor);
	$self->_movements->{$actor} = Game::Server::Movement->new_with_coeffs(
		actor => $actor,
		x => $x,
		y => $y,
		speed => Game::Config->config->{base_speed}, # TODO
		time => $self->get_time,
		map => $self->location_data->location->data->map,
	);

	return;
}

sub cancel_movement ($self, $actor)
{
	return unless exists $self->_movements->{$actor};
	$self->_process_movement(delete $self->_movements->{$actor}, $self->get_time);

	return;
}

sub _process_movement ($self, $movement, $elapsed)
{
	Game::Mechanics::Movement->move($movement, $elapsed);

	return;
}

sub _process_movements ($self)
{
	my $elapsed = $self->get_time;
	foreach my $movement (values $self->_movements->%*) {
		if (!$self->_process_movement($movement, $elapsed)) {
			delete $self->_movements->{$movement->actor};
			# TODO: notify the client to stop moving
			# TODO: error to the client
		}
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_process_movements');
};

