package Server::Action::UseAbility;

use My::Moose;
use all 'Model';
use Server::Config;

use header;

extends 'Server::GameAction';

use constant name => 'use_ability';
use constant required_state => Model::PlayerSession->STATE_PLAYING;
use constant deserializes => true;

sub validate ($self, $data)
{
	state $type = Dict [
		lore_id => LoreId,
		x => Num,
		y => Num,
	];

	$type->assert_valid($data);

	return $data;
}

sub handle ($self, $player_id, $id, $data)
{
	$self->game_process->server->use_ability($player_id, $data->%*);

	return;
}

