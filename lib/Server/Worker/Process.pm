package Server::Worker::Process;

use My::Moose;
use Mojo::IOLoop;
use Data::Dumper;

use header;

use constant LOCK_KEY => 'worker_locks';

has 'worker' => (
	is => 'ro',
	isa => Types::InstanceOf['Server::Worker'],
	weak_ref => 1,
	required => 1,
);

has 'process_id' => (
	is => 'ro',
	isa => Types::PositiveInt,
	required => 1,
);

sub _lock ($self, $uid)
{
	state $db = $self->worker->redis->db;
	return $db->hsetnx(LOCK_KEY, $uid, $self->process_id);
}

sub handle ($self, $data)
{
	my ($id, $name, @args) = $data->@*;

	return if !$self->_lock($id);

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
		$self->worker->log->error("Processing $name failed");
		$self->worker->log->debug("Error was: " . Dumper($e));
	}

	return;
}

sub do_work ($self)
{
	my $decoder = $self->worker->decoder;
	my $cb = $self->worker->redis->pubsub->listen(
		$self->worker->PUBSUB_KEY, sub ($, $data) {
			$self->handle($decoder->decode($data));
		}
	);

	Mojo::IOLoop->start;
}
