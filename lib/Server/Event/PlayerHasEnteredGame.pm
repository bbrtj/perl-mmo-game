package Server::Event::PlayerHasEnteredGame;

use My::Moose;
use all 'Model';

use header;

extends 'Server::Event::PlayerHasEnteredLocation';

use constant name => 'player_has_entered_game';

sub update_session ($self, $session, $actor)
{
	$session->set_playing($actor);

	my $lookup = Model::PlayerSessionLookup->new(id => $actor->character->name, session_id => $session->id);
	$self->cache_repo->save($lookup);

	return;
}

sub update_actor ($self, $session, $actor)
{
	$actor->variables->set_location_id($self->game_process->location_id);
	$actor->player->set_online(!!1);
	$self->models_repo->update($actor->player);

	return;
}

# same handle() as in parent

