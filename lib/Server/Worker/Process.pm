package Server::Worker::Process;

use My::Moose;

use header;

has 'worker' => (
	is => 'ro',
	isa => Types::InstanceOf ['Server::Worker'],
	weak_ref => 1,
	required => 1,
);

has 'process_id' => (
	is => 'ro',
	isa => Types::SimpleStr,
	required => 1,
);

sub do_work ($self)
{
	...;
}

