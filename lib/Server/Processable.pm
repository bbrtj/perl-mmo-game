package Server::Processable;

use My::Moose::Role;

use header;

has DI->injected('cache');
has DI->injected('channel' => 'channel_service');

sub name { ... }
sub handle { ... }
use constant disabled => 0;

sub send_to ($self, $session_id, $echo, %more)
{
	my $data = {
		%more,
		(
			defined $echo
			? (echo => ($echo isa 'Resource' ? $echo->serialize : $echo))
			: ()
		)
	};

	$self->channel->broadcast($session_id, $data);
	return;
}

