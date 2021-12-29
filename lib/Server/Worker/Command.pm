package Server::Worker::Command;

use My::Moose;
use DI;

use header;

sub send_to ($self, $user_id, $data)
{
	state $pubsub = DI->get('redis')->pubsub;
	state $encoder = DI->get('encoder');

	my $key = 'server_echo:' . $user_id;
	$pubsub->notify($key => encoder->encode($data));

	return;
}
