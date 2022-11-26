package Model::Character;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'player_id' => (
	isa => Types::Maybe [Types::ULID],
	default => undef,
);

has param 'npc_id' => (
	isa => Types::Maybe [Types::LoreId],
	default => undef,
);

has param 'class_id' => (
	isa => Types::LoreId,
);

has param 'name' => (
	isa => Types::ShortStr,
);

has param 'base_stats' => (
	isa => Types::Str,
	default => '',
);

sub is_player ($self)
{
	return defined $self->player_id;
}

__PACKAGE__->_register;

