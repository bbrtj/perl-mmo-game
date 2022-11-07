package Model::Character;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'player_id' => (
	isa => Types::Maybe [Types::ULID],
	default => sub { undef },
);

has param 'npc_id' => (
	isa => Types::Maybe [Types::LoreId],
	default => sub { undef },
);

has param 'class_id' => (
	isa => Types::LoreId,
);

has param 'name' => (
	isa => Types::NonEmptySimpleStr->where(q{ length $_ <= 32 }),
);

has param 'base_stats' => (
	isa => Types::Str,
	default => sub { '' },
);

sub is_player ($self)
{
	return defined $self->player_id;
}

__PACKAGE__->_register;

