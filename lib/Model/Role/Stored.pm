package Model::Role::Stored;

use My::Moose::Role;
use Types;

use header;

with qw(My::Moose::Role::TracksDirty);

has 'id' => (
	is => 'ro',
	isa => Types::Ulid,
	coerce => 1,
	default => sub { undef },
);
