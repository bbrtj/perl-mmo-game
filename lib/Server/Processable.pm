package Server::Processable;

use My::Moose::Role;
use DI;

use header;

has 'cache' => (
	is => 'ro',
	default => sub { DI->get('cache') },
);

has 'channel' => (
	is => 'ro',
	default => sub { DI->get('channel_service') },
);

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

