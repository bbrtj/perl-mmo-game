package Resource::X;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['X::Pub'],
);

sub serialize ($self)
{
	return {
		error => '' . $self->subject,
	};
}

