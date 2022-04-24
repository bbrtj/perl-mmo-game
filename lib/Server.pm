package Server;

use My::Moose;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json from_json);
use Data::ULID qw(ulid);
use Server::Config;

use Exception::Network::InvalidCommand;
use Exception::Network::CorruptedInput;

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

sub handle_message ($self, $id, $req_id, $type, $data)
{
	Exception::Network::CorruptedInput->throw
		if !$req_id || ref $data ne 'HASH';

	Exception::Network::InvalidCommand->throw
		unless $type && exists $self->worker->actions->{$type};

	# TODO: priorities
	$self->worker->broadcast(action => $type, $id, $req_id, $data);
	return;
}

sub connection ($self, $loop, $stream, $id)
{
	$self->log->debug('New TCP connection from ' . $stream->handle->peerhost);

	my $session = Model::PlayerSession->new;
	$self->cache->save($session);

	# TODO: dispatch map
	my $handle_feedback = sub ($data_href) {
		if ($data_href->{echo}) {
			# NOTE: this newline is essential for the client to get this data
			$stream->write(to_json($data_href->{echo}) . "\n");
		}
		if ($data_href->{refresh}) {
			$session = $self->cache->load(PlayerSession => $session->id);
		}
	};

	my @callbacks = (
		[$session->id],
		[undef],
	);

	push $_->@*, $self->channel->listen(
		$_->[0],
		$handle_feedback
	) for @callbacks;

	# react to tcp messages
	# TODO: should exceptions be caught?
	# TODO: save user game session state
	$stream->on(
		read => sub ($, $bytes) {
			return if $bytes eq 'heartbeat';

			my $msg = from_json $bytes;
			my ($id, $type, $data) = $msg->@{qw(n t d)};

			$self->handle_message($session->id, $id, $type, $data // {});
		}
	);

	$stream->on(close => sub {
		$self->channel->unlisten(@$_) for @callbacks;
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
	Mojo::IOLoop->server({
		port => $self->port,
		reuse => 1,
		# TODO: tls
	} => sub {
		unshift @_, $self;
		goto \&connection;
	});
}

sub listen ($self, $processes = 4)
{
	$self->create_forks($processes - 1, sub ($process_id) {
		$self->start;
	});

	# main process will be a server as well
	$self->start;
}

