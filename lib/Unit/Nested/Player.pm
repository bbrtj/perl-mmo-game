package Unit::Nested::Player;

use My::Moose;
use all 'Model';

use header;

extends 'Unit';

has param 'player' => (
	isa => Types::InstanceOf ['Model::Player'],
);

has param 'character' => (
	isa => Types::InstanceOf ['Model::Character'],
);

sub models ($self)
{
	return [
		$self->player,
		$self->character,
	];
}

