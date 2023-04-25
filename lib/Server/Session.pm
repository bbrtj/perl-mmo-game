package Server::Session;

use My::Moose;
use Server::Config;

use all 'X', 'Model';

use header;

has param 'server' => (
	isa => Types::InstanceOf ['Server'],
	'handles->' => {
		'cache_repo' => 'cache_repo',
		'channel_service' => 'channel_service',
		'worker' => 'worker',
		'log' => 'log',
	}
);

has param 'stream' => (
	'handles->' => {
		'send' => 'write',
	},
);

has param 'on_dropped' => (
	isa => Types::CodeRef,
);

has field 'session' => (
	writer => 1,
	default => sub {
		Model::PlayerSession->new;
	},
);

with qw(
	Server::Role::Listening
);

sub BUILD ($self, $)
{
	my $session = $self->session;
	$self->cache_repo->save($session);
	$self->_listen(
		$self->channel_service,
		$session->id,
		sub {
			$self->handle_feedback(@_);
		}
	);

	# react to tcp events
	my $stream = $self->stream;
	$stream->on(read => sub ($, $bytes) { $self->unpack_message($_) for split /\r\n/, $bytes; });
	$stream->on(close => sub { $self->dropped });
	$stream->on(error => sub ($, $err) { $self->log->error("TCP Error: $err") });
	$stream->timeout(Server::Config::GAME_SERVER_TIMEOUT);

	return;
}

sub dropped ($self)
{
	$self->log->debug("Connection dropped")
		if Server::Config::DEBUG;

	$self->worker->data_bus->broadcast('logout', $self->session->id);
	$self->_unlisten;

	$self->on_dropped->();
	return;
}

sub unpack_message ($self, $bytes)
{
	if ($bytes eq 'ping') {
		$self->send("ping\r\n");
		return;
	}

	$self->log->debug("TCP message: '$bytes'")
		if Server::Config::DEBUG;

	# check the length of $bytes to avoid getting attacked
	X::Network::CorruptedInput->throw
		if length $bytes > Server::Config::PROTOCOL_MAX_LENGTH;

	$self->handle_message(
		split Server::Config::PROTOCOL_CONTROL_CHARACTER, $bytes, 3
	);

	return;
}

# NOTE: this function needs to do the bare minimum to ensure low latency
sub handle_message ($self, $req_id, $type, $data = undef)
{
	X::Network::CorruptedInput->throw('no id or no type')
		if !$req_id || !$type;

	# $action may be either a normal or ingame action
	# (both are really the same thing but differ in where they should be passed)
	my $action = $self->worker->get_action($type);

	X::Network::InvalidAction->throw("Got $type")
		unless defined $action;

	X::Network::InvalidState->throw(sprintf "Currently %s, needs %s", $self->session->state, $action->required_state)
		unless $self->session->state == $action->required_state;

	# validate may return an object that was created from $data
	try {
		$data = $action->validate($action->deserializes && $data ? __deserialize($data) : $data);
	}
	catch ($e) {
		X::Network::CorruptedInput->throw("$e");
	}

	$self->worker->data_bus->emit($action, $self->session, ($req_id, $data));

	return;
}

sub handle_feedback ($self, $data_href)
{
	my %data = $data_href->%*;

	if (defined $data{echo}) {
		$self->send(
			join(
				Server::Config::PROTOCOL_CONTROL_CHARACTER,
				($data{id} // ''),
				($data{echo_type} // ''),
				(is_ref $data{echo} ? __serialize($data{echo}) : $data{echo}),
				)

				# NOTE: this CRLF is essential for the client to get this data
				. "\r\n",
		);
	}

	$self->set_session($self->cache_repo->load(PlayerSession => $self->session->id))
		if $data{refresh};

	$self->stream->close
		if $data{drop};

	return;
}

