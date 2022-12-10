package Server;

use My::Moose;
use Mojo::IOLoop;
use Server::Config;
use Server::Worker;
use Server::Session;

use header;

# TODO kqueue

has injected 'cache_repo';
has injected 'channel_service';

has param 'port' => (
	isa => Types::PositiveInt,
	default => sub { Server::Config::GAME_SERVER_PORT },
);

has param 'worker' => (
	constructed => ['Server::Worker'],
);

has field 'connections' => (
	isa => Types::HashRef [Types::CodeRef],
	default => sub { {} },
);

with qw(
	Server::Role::Forked
	Server::Role::Listening
);

sub connection ($self, $stream, $id)
{
	$self->log->debug('New TCP connection from ' . $stream->handle->peerhost)
		if Server::Config::DEBUG;

	# TODO: check if a player session exists, if yes then hook onto it
	# However, make sure we don't have two clients connected at once
	$self->connections->{$id} = Server::Session->new(
		server => $self,
		stream => $stream,
		on_dropped => sub {
			delete $self->connections->{$id};
		}
	);

	return;
}

sub start ($self)
{
	# listen to data that should be transmitted to all the players at once
	# (global events, announcements, server messages)
	$self->_listen(
		$self->channel_service,
		undef,
		sub {
			foreach my $session (values $self->connections->%*) {
				$session->handle_feedback(@_);
			}
		}
	);

	Mojo::IOLoop->server(
		{
			port => $self->port,
			reuse => 1,

			# TODO: tls
		} => sub ($, $stream, $id) {
			$self->connection($stream, $id);
		}
	);

	$self->_unlisten;

	return;
}

sub start_listening ($self, $processes = 4)
{
	$self->create_forks_with_parent(
		'tcp',
		$processes,
		sub ($process_id) {
			$self->start;
		},
	);

	return;
}

