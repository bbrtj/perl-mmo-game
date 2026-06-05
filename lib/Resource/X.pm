package Resource::X;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['X::Pub'],
);

use constant type => 'error';

sub generate ($self)
{
	return $self->subject;
}

