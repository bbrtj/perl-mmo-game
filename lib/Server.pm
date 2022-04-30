package Server;

use My::Moose;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json from_json);
use Server::Config;

use Exception::Network::InvalidAction;
use Exception::Network::CorruptedInput;
use Exception::Network::InvalidState;

use header;

# TODO kqueue

with qw(
	Server::Forked
);

has 'worker' => (
	is => 'ro',
);

has 'channel' => (
	is => 'ro',
);

has 'cache' => (
	is => 'ro',
);

has 'port' => (
	is => 'ro',
	default => sub { Server::Config::GAME_SERVER_PORT },
);

has 'connections' => (
	is => 'ro',
	default => sub { {} },
);

# return inlined sub that will quickly search for a matching action
sub _get_action_inlined ($self)
{
	my %map = (
		$self->worker->commands->%*,
		$self->worker->actions->%*,
	);

	return sub ($type) {
		Exception::Network::InvalidAction->throw
			unless defined $map{$type};

		return $map{$type};
	};
}

# NOTE: this function needs to do the bare minimum to ensure low latency
sub handle_message ($self, $session, $req_id, $type, $data = undef)
{
	state $actions = $self->_get_action_inlined;

	Exception::Network::CorruptedInput->throw(msg => 'no id or no type')
		if !$req_id || !$type;

	# $action may be either an action or a command
	# (both are really the same thing but differ in where they should be passed)
	my $action = $actions->($type);

	Exception::Network::InvalidState->throw
		unless $session->state eq $action->required_state;

	# validate may return an object that was created from $data
	try {
		$data = $action->validate(defined $data ? from_json($data) : undef);
	} catch ($e) {
		Exception::Network::CorruptedInput->throw(msg => $e);
	}

	if ($action->isa('Server::Action')) {
		$self->worker->broadcast_action($session->location, $type, $session->id, $req_id, $data);
	}
	else {
		$self->worker->broadcast($type, $session->id, $req_id, $data);
	}

	return;
}

sub connection ($self, $loop, $stream, $id)
{
	$self->log->debug('New TCP connection from ' . $stream->handle->peerhost);

	# TODO: check if a player session exists, if yes then hook onto it
	# However, make sure we don't have two clients connected at once
	my $session = Model::PlayerSession->new;
	$self->cache->save($session);

	my $handle_feedback = sub ($data_href) {
		my %data = $data_href->%*;

		if ($data{echo}) {
			$stream->write(
				($data{id} // '')
				. Server::Config::PROTOCOL_CONTROL_CHARACTER
				. to_json($data{echo})
				# NOTE: this newline is essential for the client to get this data
				. "\n"
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
			return if $bytes eq 'ping';

			# check the length of $bytes to avoid getting attacked
			Exception::Network::CorruptedInput->throw
				if length $bytes > Server::Config::PROTOCOL_MAX_LENGTH;

			$self->handle_message(
				$session,
				split Server::Config::PROTOCOL_CONTROL_CHARACTER, $bytes, 3
			);
		}
	);

	$self->connections->{$id} = $handle_feedback;
	my $cb = $self->channel->listen($session->id, $handle_feedback);

	$stream->on(close => sub {
		# TODO: log out from the world
		$self->channel->unlisten($session->id, $cb);
		delete $self->connections->{$id};
		$self->cache->remove($session);
	});

	$stream->on(error => sub ($, $err) {
		$self->log->error("TCP Error: $err");
	});

	$stream->timeout(Server::Config::GAME_SERVER_TIMEOUT);
	return;
}

sub start ($self)
{
	# listen to data that should be transmitted to all the players at once
	# (global events, announcements, server messages)
	my $cb = $self->channel->listen(undef, sub {
		for my $connection_cb (values $self->connections->%*) {
			$connection_cb->(@_);
		}
	});

	Mojo::IOLoop->server({
		port => $self->port,
		reuse => 1,
		# TODO: tls
	} => sub {
		unshift @_, $self;
		goto \&connection;
	});

	$self->channel->unlisten(undef, $cb);

	return;
}

sub start_listening ($self, $processes = 4)
{
	$self->create_forks('tcp', $processes - 1, sub ($process_id) {
		$self->start;
	});

	# main process will be a server as well
	$self->start;
	return;
}

