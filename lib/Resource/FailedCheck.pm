package Resource::FailedCheck;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Game::Mechanics::Check'],
);

sub serialize ($self)
{
	return {
		error => $self->subject->error,
	};
}

