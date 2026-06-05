package Server;

use My::Moose;
use Server::Config;
use Server::Worker;
use Server::Session;

use IO::Async::OS;
use Socket qw(SO_REUSEPORT SO_REUSEADDR);

use header;

has injected 'cache_repo';
has injected 'channel_service';

has param 'port' => (
	isa => PositiveInt,
	default => sub { Server::Config::GAME_SERVER_PORT },
);

has param 'worker' => (
	constructed => ['Server::Worker'],
);

has field 'connections' => (
	isa => HashRef [CodeRef],
	default => sub { {} },
);

with qw(
	Server::Role::Forked
	Server::Role::Listening
);

sub build_message ($self, $data_href)
{
	my $echo = $data_href->{echo};

	return join(
		Server::Config::PROTOCOL_CONTROL_CHARACTER,
		($data_href->{id} // ''),
		($data_href->{echo_type} // ''),
		(is_ref $echo ? __serialize($echo) : $echo),
		)

		# NOTE: this CRLF is essential for the client to get this data
		. "\r\n"
		;
}

sub handle_global_feedback ($self, $data_href)
{
	my @recipients;

	if ($data_href->{sessions}) {
		@recipients = grep { defined } $self->connections->@{$data_href->{sessions}->@*};
	}
	else {
		@recipients = values $self->connections->%*;
	}

	my $message = $self->build_message($data_href);
	foreach my $session (@recipients) {
		$session->send($message);
	}
}

sub connection ($self, $stream)
{
	$self->log->debug('New TCP connection from ' . $stream->read_handle->peerhost)
		if Server::Config::DEBUG;

	my $id;
	my $connection = Server::Session->new(
		server => $self,
		stream => $stream,
		on_dropped => sub {
			delete $self->connections->{$id};
		}
	);

	$id = $connection->id;
	$self->connections->{$id} = $connection;

	return;
}

sub start ($self, $processes = 4)
{
	$self->create_forks(
		'tcp',
		$processes,
		sub ($process_id) {

			# listen to data that should be transmitted to all the players at once
			# (global events, announcements, server messages)
			$self->_listen(
				$self->channel_service,
				undef,
				sub {
					$self->handle_global_feedback(@_);
				}
			);

			my ($family, $socktype, $proto, $address) = IO::Async::OS->extract_addrinfo(
				{
					family => 'inet',
					socktype => 'stream',
					port => $self->port,
				}
			);

			my $sock;
			$sock = IO::Async::OS->socket($family, $socktype, $proto) or die $!;
			$sock->blocking(0);
			$sock->sockopt(SO_REUSEADDR, 1) or die $!;
			$sock->sockopt(SO_REUSEPORT, 1) or die $!;
			$sock->bind($address) or die $!;
			$sock->listen(64) or die $!;

			$self->loop->listen(
				handle => $sock,
				on_stream => sub ($stream) {
					$self->connection($stream);
				},
			)->get;

			$self->loop->run;
			$self->_unlisten;
		},
	);

	$self->process_setup;
	return;
}

