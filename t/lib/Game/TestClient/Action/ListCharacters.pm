package Game::TestClient::Action::ListCharacters;

use My::Moose;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

has injected 'units_repo';

use constant requires => ['Login'];

sub send_queue ($self)
{
	return (
		['list_characters'],
	);
}

sub receive_queue ($self)
{
	return (
		Resource::CharacterList->new($self->units_repo->load_user($self->client->actor->player->user_id)),
	);
}

