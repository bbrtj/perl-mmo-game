package Server::Command;

use My::Moose;
use DI;

use header;

sub name { ... }
sub interval { ... }
sub handle { ... }

use constant disabled => 0;

sub send_to ($self, $id, $data)
{
	state $channel = DI->get('channel_service');

	$channel->broadcast($id, $data);
	return;
}
