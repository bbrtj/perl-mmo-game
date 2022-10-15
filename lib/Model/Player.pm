package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'user_id' => (
	isa => Types::Ulid,
);

has param 'online' => (
	isa => Types::Bool,
	default => sub { 0 },
);

has param 'last_online' => (
	coerce => Types::Maybe [Types::DateTime],
	default => sub { undef },
);

has param 'created_at' => (
	coerce => Types::DateTime,
	default => sub { time },
);

__PACKAGE__->_register;

