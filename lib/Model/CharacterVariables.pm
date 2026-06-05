package Model::CharacterVariables;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'experience' => (
	isa => PositiveOrZeroInt,
	default => 0,
);

has param 'location_id' => (
	isa => LoreId,
);

has param ['pos_x', 'pos_y'] => (
	isa => Num,
);

has param 'health' => (
	isa => Num,
);

has param 'energy' => (
	isa => Num,
);

sub xy ($self)
{
	return ($self->pos_x, $self->pos_y);
}

__PACKAGE__->_register;

