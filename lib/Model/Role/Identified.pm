package Model::Role::Identified;

use My::Moose::Role;
use Data::ULID qw(ulid);

use header;

has param 'id' => (
	coerce => Types::Ulid,
	default => sub { undef },
);

requires qw(dummy);

around dummy => sub ($orig, $self, %args) {
	return $self->$orig(id => ulid, %args);
};

