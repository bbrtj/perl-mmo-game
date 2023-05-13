package Game::Object::Actor::Stats;

use My::Moose;

use header;

has field 'movement' => (
	isa => Types::InstanceOf['Game::Object::Movement'],
	writer => -hidden,
	clearer => 1,
);

# angle is needed because movement is optional
has field 'angle' => (
	writer => 1,
	default => 0,
);

has field 'speed' => (
	writer => 1,
	default => Game::Config->config->{base_speed}, # TODO
);

sub set_movement ($self, $movement)
{
	$self->_set_movement($movement);
	$self->set_angle($movement->angle);
	return;
}

