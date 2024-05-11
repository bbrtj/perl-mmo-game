package Server::Role::CanSendData;

use My::Moose::Role;

use header;

has injected 'channel_service';

sub send_to ($self, $session_id, $echo, %data)
{
	if ($echo isa 'Resource') {
		my @to_process = $echo;

		while (my $next = shift @to_process) {
			$data{echo} = $next->serialized;
			$data{echo_type} = $next->type;
			push @to_process, $next->next_resources->@*;

			$self->channel_service->broadcast($session_id, \%data);
		}
	}
	else {
		$data{echo} = $echo;
		$self->channel_service->broadcast($session_id, \%data);
	}

	return;
}

sub send_to_all ($self, $echo, %data)
{
	return $self->send_to(undef, $echo, %data);
}

