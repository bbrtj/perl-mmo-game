package Server::Command;

use My::Moose;
use DI;

use header;

sub name { ... }
sub interval { ... }

use constant disabled => 0;

sub send_to ($self, $user_id, $data)
{
	state $channel = DI->get('channel_service');

	$channel->broadcast($user_id, $data);
	return;
}
