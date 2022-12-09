package Server::Role::CanSendData;

use My::Moose::Role;

use header;

has injected 'channel_service';

sub send_to ($self, $session_id, $echo, %more)
{
	my $data = {
		%more,
		(
			defined $echo
			? (echo => ($echo isa 'Resource' ? $echo->serialized : $echo))
			: ()
		)
	};

	$self->channel_service->broadcast($session_id, $data);
	return;
}

