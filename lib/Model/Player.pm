package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'user_id' => (
	isa => Types::ULID,
);

has param 'online' => (
	isa => Types::Bool,
	default => !!0,
);

has param 'last_online' => (
	coerce => Types::Maybe [Types::DateTime],
	default => undef,
);

has param 'created_at' => (
	coerce => Types::DateTime,
	default => sub { time },
);

__PACKAGE__->_register;

