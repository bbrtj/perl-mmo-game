package Model::Role::Stored;

use My::Moose::Role;
use Types;

use header;

has 'id' => (
	is => 'ro',
	isa => Types::Ulid,
	coerce => 1,
	default => sub { undef },
);

# dirty columns - for updating the model in db
has '_dirty' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
	lazy => 1,
	clearer => '_clear_dirty',
);
