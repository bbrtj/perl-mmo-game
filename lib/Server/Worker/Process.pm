package Server::Worker::Process;

use My::Moose;

use header;

has param 'worker' => (
	isa => Types::InstanceOf ['Server::Worker'],
	weak_ref => 1,
);

has param 'process_id' => (
	isa => Types::SimpleStr,
);

sub do_work ($self)
{
	...;
}

