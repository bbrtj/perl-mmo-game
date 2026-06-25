package Server::Process::Jobs;

use My::Moose;
use Server::Config;

use header;

use constant LOCK_KEY => 'job_locks';

extends 'Server::Process';

with qw(
	Server::Role::Listening
	Server::Role::CanSendData
);

sub _lock ($self, $ulid)
{
	state $db = DI->get('redis')->redis;
	return $db->hsetnx(LOCK_KEY, $ulid, $self->process_id);
}

sub handle ($self, $data)
{
	my ($ulid, $name, $session_id, $id, @args) = $data->@*;

	return if !$self->_lock($ulid)->get;

	$self->log->debug("Got a job: $name")
		if Server::Config::DEBUG;

	my $instance = $self->worker->get_processable($name);

	if (!defined $instance || $instance->does('Server::Role::WithGameProcess')) {
		$self->log->error("Unknown job name $name");
		return;
	}

	$self->log->debug('Process ' . $self->process_id . ": processing $name");
	try {
		$instance->handle($session_id, $id, @args);
	}
	catch ($e) {
		if ($e isa 'X::Pub') {
			$self->send_to(
				$session_id,
				Resource::X->new(subject => $e),
				id => $id,
			);
		}
		else {
			$self->log->error("Processing job $name failed: $e");
			$self->log->debug("Error was: " . My::Dumper->dd($e))
				if Server::Config::DEBUG;
		}
	}

	return;
}

sub do_work ($self, $loop)
{
	$self->_listen(
		$self->worker->data_bus,
		undef,
		sub ($data) {
			$self->handle($data);
		}
	);

	return;
}

sub finish_work ($self)
{
	$self->_unlisten;
	return;
}

