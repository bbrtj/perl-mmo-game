package Server::Action::Yell;

use My::Moose;
use all 'Model';

use header;

extends 'Server::GameAction';

use constant name => 'yell';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	return $data;
}

sub handle ($self, $player_id, $id, $message)
{
	$self->game_process->server->chat_yell($player_id, $message);

	return;
}

