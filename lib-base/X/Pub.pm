package X::Pub;

use v5.38;
use My::Moose;

extends 'X';

# in this context, this is a string id to be translated
has extended 'msg' => (
	builder => 1,
);

sub build_msg
{
	return 'err';
}

sub stringify ($self, @)
{
	return $self->msg;
}

