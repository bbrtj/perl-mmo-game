package Server::Command;

use My::Moose;
use DI;

use header;

sub name { ... }
sub interval { ... }
sub handle { ... }

use constant disabled => 0;

sub cache { state $cache = DI->get('cache') }

sub send_to ($self, $id, $data)
{
	state $channel = DI->get('channel_service');

	$data = {echo => $data->hash}
		if $data isa 'Resource';

	$channel->broadcast($id, $data);
	return;
}
