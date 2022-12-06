package X::Pub;

use v5.36;
use My::Moose;

extends 'X';

# in this context, this is a string id to be translated
has extended 'msg' => (
	builder => 1,
);

sub build_msg { 'err' }

sub stringify ($self, @)
{
	return $self->msg;
}

1;

