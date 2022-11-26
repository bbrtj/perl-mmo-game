package Model::Role::Identified;

use My::Moose::Role;
use Data::ULID qw(ulid);

use header;

has param 'id' => (
	isa => Types::ULID,
	default => sub { ulid },
);

requires qw(dummy);

around dummy => sub ($orig, $self, %args) {
	return $self->$orig(id => ulid, %args);
};

