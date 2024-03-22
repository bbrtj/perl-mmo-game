package Server::Action::Say;

use My::Moose;
use all 'Model';

use header;

extends 'Server::GameAction';

use constant name => 'say';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	return $data;
}

sub handle ($self, $player_id, $id, $message)
{
	$self->game_process->server->chat_say($player_id, $message);

	return;
}

