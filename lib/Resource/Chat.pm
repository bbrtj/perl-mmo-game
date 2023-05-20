package Resource::Chat;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has param 'message' => (
	isa => Types::Str,
);

has param 'whisper' => (
	isa => Types::Bool,
	default => !!0,
);

use constant type => 'chat';

sub generate ($self)
{
	return {
		id => $self->actor->id,
		message => $self->message,
		whisper => $self->whisper,
	};
}

