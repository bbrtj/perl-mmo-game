package Server::Action;

use My::Moose;

use header;

extends 'Server::Command';

sub handle ($self, $session_id, $id, $data)
{
	my $returned = inner;

	$returned = {echo => $returned->hash}
		if $returned isa 'Resource';

	$self->send_to($session_id, $returned);
	return;
}
