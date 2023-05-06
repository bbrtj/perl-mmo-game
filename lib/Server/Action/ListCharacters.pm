package Server::Action::ListCharacters;

use My::Moose;
use Resource::CharacterList;
use all 'Model';

use header;

extends 'Server::Action';

has injected 'units_repo';

use constant name => 'list_characters';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

sub handle ($self, $session_id, $id, $)
{
	my $session = $self->cache_repo->load(PlayerSession => $session_id);
	my $unit = $self->units_repo->load_user($session->user_id);

	$self->send_to(
		$session_id,
		Resource::CharacterList->new(subject => $unit),
		id => $id
	);

	return;
}

