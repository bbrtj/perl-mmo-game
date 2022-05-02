package Server::Command::Logout;

use My::Moose;
use Model;

use header;

extends 'Server::Command';

use constant name => 'logout';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

# NOTE: this logs out of the character selection screen, not of the game

sub handle ($self, $session_id, $id, $data)
{
	my $session = $self->cache->load(PlayerSession => $session_id);

	$session->clear_user_id;
	$session->set_state($session->STATE_NEW);
	$self->cache->save($session);

	return $self->send_to(
		$session_id,
		undef,
		refresh => 1,
	);
}

