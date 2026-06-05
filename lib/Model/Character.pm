package Model::Character;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'player_id' => (
	isa => Maybe [ULID],
	default => undef,
);

has param 'npc_id' => (
	isa => Maybe [LoreId],
	default => undef,
);

has param 'class_id' => (
	isa => LoreId,
);

has param 'name' => (
	isa => ShortStr,
);

has param 'base_stats' => (
	isa => Str,
	default => '',
);

sub is_player ($self)
{
	return defined $self->player_id;
}

__PACKAGE__->_register;

