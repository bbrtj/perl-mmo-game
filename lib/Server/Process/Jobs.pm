package Server::Process::Jobs;

use My::Moose;
use Mojo::IOLoop;
use Data::Dumper;
use Server::Config;

use header;

use constant LOCK_KEY => 'job_locks';

extends 'Server::Process';

with qw(
	Server::Role::Listening
);

sub _lock ($self, $ulid)
{
	state $db = DI->get('redis')->db;
	return $db->hsetnx(LOCK_KEY, $ulid, $self->process_id);
}

sub handle ($self, $data)
{
	my ($ulid, $name, @args) = $data->@*;

	return if !$self->_lock($ulid);

	$self->worker->log->debug("Got a job: $name")
		if Server::Config::DEBUG;

	my $instance = $self->worker->get_processable($name);

	if (!defined $instance || $instance->does('Server::Role::WithGameProcess')) {
		$self->worker->log->error("Unknown job name $name");
		return;
	}

	$self->worker->log->debug('Process ' . $self->process_id . ": processing $name");
	try {
		$instance->handle(@args);
	}
	catch ($e) {
		$self->worker->log->error("Processing job $name failed: $e");
		$self->worker->log->debug("Error was: " . Dumper($e));
	}

	return;
}

sub do_work ($self)
{
	$self->_listen(
		$self->worker->data_bus,
		undef,
		sub ($data) {
			$self->handle($data);
		}
	);

	Mojo::IOLoop->start;

	$self->_unlisten;
	return;
}
