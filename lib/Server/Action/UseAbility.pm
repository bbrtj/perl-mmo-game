package Server::Action::UseAbility;

use My::Moose;
use all 'Model';
use Server::Config;
use Game::Mechanics::Check::Map;

use header;

extends 'Server::GameAction';

use constant name => 'use_ability';
use constant required_state => Model::PlayerSession->STATE_PLAYING;
use constant deserializes => !!1;

sub validate ($self, $data)
{
	state $type = Types::Dict [
		ability => Types::Str,
		x => Types::Optional [Types::Num],
		y => Types::Optional [Types::Num],
	];

	$type->assert_valid($data);

	return $data;
}

before handle => sub ($self, $player_id, $id, $position) {

	# TODO: check if player can use the ability now (here or in handle)
};

sub handle ($self, $player_id, $id, $data)
{
	$self->game_process->server->use_ability($player_id, $data->%*);

	return;
}

