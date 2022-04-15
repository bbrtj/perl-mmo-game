package Unit::Nested::Player;

use My::Moose;
use Model;
use Types;

use header;

extends 'Unit';

has 'player' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Player'],
);

has 'character' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Character'],
);

sub models ($self)
{
	return [
		$self->player,
		$self->character,
	];
}

