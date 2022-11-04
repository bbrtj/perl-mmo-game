package Server;

use My::Moose;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json from_json);
use Server::Config;
use Server::Worker;

# use Sub::HandlesVia;

use X::Network::InvalidAction;
use X::Network::CorruptedInput;
use X::Network::InvalidState;

use header;

# TODO kqueue

has injected 'cache';
has injected 'channel' => as => 'channel_service';

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

# NOTE: this function needs to do the bare minimum to ensure low latency
sub handle_message ($self, $session, $req_id, $type, $data = undef)
{
	X::Network::CorruptedInput->throw(msg => 'no id or no type')
		if !$req_id || !$type;

	# $action may be either a normal or ingame action
	# (both are really the same thing but differ in where they should be passed)
	my $action = $self->worker->get_action($type);

	X::Network::InvalidAction->throw(msg => "Got $type")
		unless defined $action;

	X::Network::InvalidState->throw(msg => sprintf "Currently %s, needs %s", $session->state, $action->required_state)
		unless $session->state eq $action->required_state;

	# validate may return an object that was created from $data
	try {
		$data = $action->validate($action->deserializes && $data ? from_json($data) : $data);
	}
	catch ($e) {
		X::Network::CorruptedInput->throw(msg => "$e");
	}

	$self->worker->data_bus->emit($action, $session, ($req_id, $data));

	return;
}

sub connection ($self, $loop, $stream, $id)
{
	$self->log->debug('New TCP connection from ' . $stream->handle->peerhost)
		if Server::Config::DEBUG;

	# TODO: check if a player session exists, if yes then hook onto it
	# However, make sure we don't have two clients connected at once
	my $session = Model::PlayerSession->new;
	$self->cache->save($session);

	my $handle_feedback = sub ($data_href) {
		my %data = $data_href->%*;

		if (defined $data{echo}) {
			$stream->write(
				join '',
					($data{id} // ''),
					Server::Config::PROTOCOL_CONTROL_CHARACTER,
					(is_ref $data{echo} ? to_json($data{echo}) : $data{echo}),

					# NOTE: this newline is essential for the client to get this data
					"\n",
			);
		}

		if ($data{refresh}) {
			$session = $self->cache->load(PlayerSession => $session->id);
		}
	};

	# react to tcp messages
	# TODO: should exceptions be caught?
	$stream->on(
		read => sub ($, $bytes) {
			$bytes =~ s/\r?\n?\Z//;

			if ($bytes eq 'ping') {
				$stream->write('ping');
				return;
			}

			$self->log->debug("TCP message: '$bytes'")
				if Server::Config::DEBUG;

			# check the length of $bytes to avoid getting attacked
			X::Network::CorruptedInput->throw
				if length $bytes > Server::Config::PROTOCOL_MAX_LENGTH;

			$self->handle_message(
				$session,
				split Server::Config::PROTOCOL_CONTROL_CHARACTER, $bytes, 3
			);
		}
	);

	$self->connections->{$id} = $handle_feedback;
	my $cb = $self->channel->listen($session->id, $handle_feedback);

	$stream->on(
		close => sub {

			# TODO: log out from the world
			$self->channel->unlisten($session->id, $cb);
			delete $self->connections->{$id};
			$self->cache->remove($session);
		}
	);

	$stream->on(
		error => sub ($, $err) {
			$self->log->error("TCP Error: $err");
		}
	);

	$stream->timeout(Server::Config::GAME_SERVER_TIMEOUT);
	return;
}

sub start ($self)
{
	# listen to data that should be transmitted to all the players at once
	# (global events, announcements, server messages)
	$self->_listen(
		$self->channel,
		undef,
		sub {
			foreach my $connection_cb (values $self->connections->%*) {
				$connection_cb->(@_);
			}
		}
	);

	Mojo::IOLoop->server(
		{
			port => $self->port,
			reuse => 1,

			# TODO: tls
		} => sub {
			unshift @_, $self;
			goto \&connection;
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

