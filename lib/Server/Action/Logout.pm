package Server::Action::Logout;

use My::Moose;
use all 'Model';

use header;

extends 'Server::Action';

use constant name => 'logout';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

# NOTE: this logs out of the character selection screen, not of the game

sub handle ($self, $session_id, $id, $)
{
	my $session = $self->cache_repo->load(PlayerSession => $session_id);

	$session->clear_user_id;
	$session->set_state($session->STATE_NEW);
	$self->cache_repo->save($session);

	return $self->send_to(
		$session_id,
		undef,
		refresh => 1,
	);
}

