package Web::Message;

use My::Moose;
use Exception::WebSocket::InvalidCommand;
use Exception::WebSocket::CorruptedInput;
use Server::Config;
use DI;

use header;

sub handle ($self, $id, $type, $user, $data)
{
	Exception::WebSocket::CorruptedInput->throw
		if !$id || ref $data ne 'HASH';

	Exception::WebSocket::InvalidCommand->throw
		unless $type && Server::Config->actions->{$type};

	state $worker = DI->get('worker');
	$worker->enqueue("($type)" => [$id, $user->id, $data]);

	return;
}
