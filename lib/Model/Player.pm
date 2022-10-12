package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has 'user_id' => (
	is => 'ro',
	isa => Types::Ulid,
	required => 1,
);

has 'online' => (
	is => 'ro',
	isa => Types::Bool,
	default => sub { 0 },
);

has 'last_online' => (
	is => 'ro',
	isa => Types::Maybe [Types::DateTime],
	coerce => 1,
	default => sub { undef },
);

has 'created_at' => (
	is => 'ro',
	isa => Types::DateTime,
	coerce => 1,
	default => sub { time },
);

__PACKAGE__->_register;

