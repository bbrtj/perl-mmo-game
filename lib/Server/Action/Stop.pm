package Server::Action::Stop;

use My::Moose;
use all 'Model';

use header;

extends 'Server::GameAction';

use constant name => 'stop';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub handle ($self, $player_id, $id, $)
{
	$self->game_process->server->cancel_movement($player_id);

	return;
}

