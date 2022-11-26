package Model::CharacterVariables;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'experience' => (
	isa => Types::PositiveOrZeroInt,
	default => 0,
);

has param 'location_id' => (
	isa => Types::LoreId,
);

has param ['pos_x', 'pos_y'] => (
	isa => Types::Num,
);

has param 'health' => (
	isa => Types::Num,
);

has param 'energy' => (
	isa => Types::Num,
);

__PACKAGE__->_register;

