package Server::Job::Logout;

use My::Moose;
use all 'Model';

use header;

extends 'Server::Job';

with qw(Server::Role::HandlesLogin);

use constant name => 'logout';
use constant interval => undef;

sub handle ($self, $session_id)
{
	$self->logout($session_id);

	return;
}

