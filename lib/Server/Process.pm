package Server::Process;

use My::Moose;

use header;

has param 'worker' => (
	isa => InstanceOf ['Server::Worker'],
	weak_ref => 1,
	handles => [qw(log)],
);

has param 'process_id' => (
	isa => SimpleStr,
);

sub do_work ($self, $loop)
{
	...;
}

