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
	if ($message =~ m{\A/}) {
		if ($message =~ s{\A/y(ell)? }{}i) {
			$self->game_process->server->chat_yell($player_id, $message);
		}
		if ($message =~ s{\A/p(riv)? (\w+) }{}i) {
			$self->game_process->server->chat_private($player_id, $2, $message);
		}

		# TODO: error
	}
	else {
		$self->game_process->server->chat_say($player_id, $message);
	}

	return;
}

