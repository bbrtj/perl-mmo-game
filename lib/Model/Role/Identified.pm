package Model::Role::Identified;

use My::Moose::Role;

use header;

has param 'id' => (
	coerce => Types::Ulid,
	default => sub { undef },
);

