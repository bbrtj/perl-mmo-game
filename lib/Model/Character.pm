package Model::Character;

use My::Moose;
use Types;

use header;

extends 'Model';

with 'Model::Role::Stored';

has 'player_id' => (
	is => 'ro',
	isa => Types::Maybe [Types::Ulid],
	default => sub { undef },
);

has 'npc_id' => (
	is => 'ro',
	isa => Types::Maybe [Types::Ulid],
	default => sub { undef },
);

has 'class_id' => (
	is => 'ro',
	isa => Types::LoreId,
	required => 1,
);

has 'name' => (
	is => 'ro',
	isa => Types::NonEmptySimpleStr->where(q{ length $_ <= 32 }),
	required => 1,
);

has 'stats' => (
	is => 'ro',
	isa => Types::NonEmptySimpleStr,
	required => 1,
);

sub is_player ($self)
{
	return defined $self->player_id;
}

__PACKAGE__->_register;
