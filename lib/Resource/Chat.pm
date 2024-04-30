package Resource::Chat;

use My::Moose;

use header;

use enum qw(SAY YELL WHISPER);
use constant CHAT_TYPES => [SAY .. WHISPER];

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has param 'message' => (
	isa => Types::Str,
);

has param 'chat_type' => (
	isa => Types::Enum [@{+CHAT_TYPES}],
	default => SAY,
);

use constant type => 'chat';

sub generate ($self)
{
	return {
		id => $self->subject->id,
		message => $self->message,
		type => $self->chat_type,
	};
}

