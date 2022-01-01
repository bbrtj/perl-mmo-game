package Server::Action;

use My::Moose;

use header;

extends 'Server::Command';

sub handle ($self, $id, $user_id, $data)
{
	my $returned = inner;

	$returned = $returned->hash
		if $returned isa 'Resource';

	$self->send_to($user_id, $returned);
	return;
}
