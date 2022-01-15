package Server;

use My::Moose;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json from_json);
use Util::H2O;

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

sub handle_message ($self, $id, $type, $user_id, $data)
{
	Exception::Network::CorruptedInput->throw
		if !$id || ref $data ne 'HASH';

	Exception::Network::InvalidCommand->throw
		unless $type && exists $self->worker->actions->{$type};

	# TODO: priorities
	$self->worker->broadcast(action => $type, $id, $user_id, $data);
	return;
}

sub connection ($self, $loop, $stream, $id)
{
	my sub init_session ($session, $user, $lang) {
		$session->user($user);
		$session->lang($lang);

		$session->stream->timeout(TIMEOUT_SEC);

		my @callbacks = (
			[$session->user],
			[undef],
		);

		for my $aref (@callbacks) {
			push $aref->@*, $self->channel->listen(
				$aref->[0],
				sub ($struct) {
					$self->reply($struct);
				}
			);
		}

		$stream->on(close => sub {
			$self->channel->unlisten(@$_) for @callbacks;
		});
	}

	my $session = h2o {
		stream => $stream,
		user => undef,
		lang => undef,
		init => \&init_session,
	};

	# react to tcp messages
	# TODO: should exceptions be caught?
	# TODO: save user game session state
	$stream->on(
		read => sub ($, $bytes) {
			return if $bytes eq 'ping';

			# TODO: login
			my $msg = from_json $bytes;
			my ($id, $type, $data) = $msg->@{qw(n t d)};

			$self->handle_message($id, $type, $session->user, $data // {});
		}
	);

	$stream->on(error => sub ($, $err) {
		$self->log->error("TCP Error: $err");
	});

	return;
}

sub reply ($self, $stream, $struct)
{
	local $i18n::CURRENT_LANG = $stream->lang;
	return $stream->write(to_json($struct));
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

