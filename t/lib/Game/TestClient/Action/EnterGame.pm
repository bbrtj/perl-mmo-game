package Game::TestClient::Action::EnterGame;

use My::Moose;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

has injected 'lore_data_repo';

use constant requires => ['Login'];

sub send_queue ($self)
{
	return (
		['enter_game', $self->client->actor->player->id],
	);
}

sub receive_queue ($self)
{
	return (
		'1',
		Resource::LocationData->new($self->lore_data_repo->load($self->client->actor->variables->location_id))
	);
}

