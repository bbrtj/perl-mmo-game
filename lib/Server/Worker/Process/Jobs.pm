package Server::Worker::Process::Jobs;

use My::Moose;
use Mojo::IOLoop;
use Data::Dumper;

use header;

use constant LOCK_KEY => 'job_locks';

extends 'Server::Worker::Process';

sub _lock ($self, $ulid)
{
	state $db = DI->get('redis')->db;
	return $db->hsetnx(LOCK_KEY, $ulid, $self->process_id);
}

sub handle ($self, $data)
{
	my ($ulid, $name, @args) = $data->@*;

	return if !$self->_lock($ulid);

	my $instance = $self->worker->get_processable($name);

	if (!defined $instance || $instance isa Server::GameAction) {
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
	my $cb = $self->worker->channel->listen(
		undef,
		sub ($data) {
			$self->handle($data);
		}
	);

	Mojo::IOLoop->start;

	$self->worker->channel->unlisten(undef, $cb);
	return;
}

