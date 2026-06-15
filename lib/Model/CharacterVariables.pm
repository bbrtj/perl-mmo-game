package Model::CharacterVariables;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'experience' => (
	isa => PositiveOrZeroInt,
	default => 0,
	traits => [qw(Stored)],
);

has param 'location_id' => (
	isa => LoreId,
	traits => [qw(Stored)],
);

has param ['pos_x', 'pos_y'] => (
	isa => Num,
	traits => [qw(Stored)],
);

has param 'health' => (
	isa => Num,
	traits => [qw(Stored)],
);

has param 'energy' => (
	isa => Num,
	traits => [qw(Stored)],
);

sub xy ($self)
{
	return ($self->pos_x, $self->pos_y);
}

__PACKAGE__->_register;

