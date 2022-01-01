package Web::Message;

use My::Moose;
use Exception::WebSocket::InvalidCommand;
use Exception::WebSocket::CorruptedInput;
use Server::Config;
use DI;

use header;

sub handle ($self, $id, $type, $user_id, $data)
{
	Exception::WebSocket::CorruptedInput->throw
		if !$id || ref $data ne 'HASH';

	state $worker = DI->get('worker');
	Exception::WebSocket::InvalidCommand->throw
		unless $type && exists $worker->actions->{$type};

	$worker->broadcast(action => $type, $id, $user_id, $data);

	return;
}
