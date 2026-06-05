package Model::Role::Identified;

use My::Moose::Role;

use header;

has param 'id' => (
	isa => ULID,
	default => sub { Types::ULID::ulid },
);

requires qw(dummy);

around dummy => sub ($orig, $self, %args) {
	return $self->$orig(id => Types::ULID::ulid, %args);
};

