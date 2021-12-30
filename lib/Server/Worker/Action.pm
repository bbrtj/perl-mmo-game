package Server::Worker::Action;

use My::Moose;

use header;

extends 'Server::Worker::Command';

sub handle ($self, $job, $id, $user_id, $data)
{
	my $returned = inner;

	$returned = $returned->hash
		if $returned isa 'Resource';

	$self->send_to($user_id, $returned);
	return;
}
