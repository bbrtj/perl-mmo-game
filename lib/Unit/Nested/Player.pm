package Unit::Nested::Player;

use My::Moose;
use all 'Model';

use header;

extends 'Unit';

has param 'player' => (
	isa => InstanceOf ['Model::Player'],
);

has param 'character' => (
	isa => InstanceOf ['Model::Character'],
);

sub models ($self)
{
	return [
		$self->player,
		$self->character,
	];
}

