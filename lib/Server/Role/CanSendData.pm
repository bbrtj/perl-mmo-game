package Server::Role::CanSendData;

use My::Moose::Role;

use header;

has injected 'channel_service';

sub send_to ($self, $session_id, $echo, %data)
{
	if (defined $echo) {
		if ($echo isa 'Resource') {
			$data{echo} = $echo->serialized;
			$data{echo_type} = $echo->type;
		}
		else {
			$data{echo} = $echo;
		}
	}

	$self->channel_service->broadcast($session_id, \%data);
	return;
}

sub send_to_all ($self, $echo, %data)
{
	return $self->send_to(undef, $echo, %data);
}

