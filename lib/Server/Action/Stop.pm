package Server::Action::Stop;

use My::Moose;
use all 'Model';
use Server::Config;
use Game::Mechanics::Check::Map;

use header;

extends 'Server::GameAction';

use constant name => 'stop';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub handle ($self, $player_id, $id, $position)
{
	$self->game_process->server->cancel_movement($player_id);

	return;
}

