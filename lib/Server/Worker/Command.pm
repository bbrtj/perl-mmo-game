package Server::Worker::Command;

use My::Moose;
use DI;

use header;

sub send_to ($self, $user_id, $data)
{
	state $channel = DI->get('channel_service');

	$channel->broadcast($user_id, $data);
	return;
}
