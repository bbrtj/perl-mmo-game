package Server;

use My::Moose;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json from_json);
use Util::H2O;
use Data::ULID qw(ulid);

use Exception::Network::InvalidCommand;
use Exception::Network::CorruptedInput;

use header;

# TODO kqueue

use constant TIMEOUT_SEC => 120;

with qw(
	Server::Forked
);

has 'worker' => (
	is => 'ro',
);

has 'channel' => (
	is => 'ro',
);

has 'port' => (
	is => 'ro',
	default => sub { 14832 },
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

	my $session = h2o {
		id => ulid,
		user_id => undef,
		lang => 'pl',
	};

	# TODO: dispatch map
	my $handle_feedback = sub ($data_href) {
		if ($data_href->{echo}) {
			local $i18n::CURRENT_LANG = $session->lang;

			# NOTE: this newline is essential for the client to get this data
			$stream->write(to_json($data_href->{echo}) . "\n");
		}
		if ($data_href->{login}) {
			$session->user_id($data_href->{login});
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
	});

	$stream->on(error => sub ($, $err) {
		$self->log->error("TCP Error: $err");
	});

	$stream->timeout(TIMEOUT_SEC);
	return;
}

sub start ($self)
{
	# TODO: catch IOLoop errors and log them
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

