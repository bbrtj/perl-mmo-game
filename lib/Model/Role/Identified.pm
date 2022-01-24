package Model::Role::Identified;

use My::Moose::Role;
use Types;

use header;

has 'id' => (
	is => 'ro',
	isa => Types::Ulid,
	coerce => 1,
	default => sub { undef },
);
