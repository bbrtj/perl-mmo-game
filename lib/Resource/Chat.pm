package Resource::Chat;

use My::Moose;

use header;

use enum qw(SAY YELL PRIVATE);
use constant CHAT_TYPES => [SAY .. PRIVATE];

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has option 'sent_to' => (
	isa => Types::Str,
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
		($self->has_sent_to ? (sent_to => $self->sent_to) : ()),
	};
}

