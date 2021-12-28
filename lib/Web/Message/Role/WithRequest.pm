package Web::Message::Role::WithRequest;

use My::Moose::Role;
use Types;
use Exception::WebSocket::CorruptedInput;

use header;

has 'request_id' => (
	is => 'rw',
	isa => Types::Str,
);

before handle => sub ($self, $user, $data) {
	Exception::WebSocket::CorruptedInput->throw
		unless exists $data->{id};
	$self->request_id($data->{id});

	return;
};
