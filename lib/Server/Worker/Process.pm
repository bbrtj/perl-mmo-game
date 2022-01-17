package Server::Worker::Process;

use My::Moose;
use Mojo::IOLoop;
use Data::Dumper;

use header;

use constant LOCK_KEY => 'worker_locks';

has 'worker' => (
	is => 'ro',
	isa => Types::InstanceOf ['Server::Worker'],
	weak_ref => 1,
	required => 1,
);

has 'process_id' => (
	is => 'ro',
	isa => Types::PositiveInt,
	required => 1,
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

	(my $type, $name) = split /:/, $name;
	$type .= 's';

	if (!$self->worker->can($type)) {
		$self->worker->log->error("Unknown worker job type $type");
		return;
	}

	my $instance = $self->worker->$type->{$name};

	if (!defined $instance) {
		$self->worker->log->error("Unknown worker job name $name of type $type");
		return;
	}

	$self->worker->log->debug('Process ' . $self->process_id . ": ($type) processing $name");
	try {
		$instance->handle(@args);
	}
	catch ($e) {
		$self->worker->log->error("Processing $name failed: $e");
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
}
