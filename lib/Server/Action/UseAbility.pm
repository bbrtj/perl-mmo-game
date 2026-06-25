package Server::Action::UseAbility;

use My::Moose;
use all 'Model';
use Server::Config;
use Utils qw(transport_floats_rev);

use header;

extends 'Server::GameAction';

use constant name => 'use_ability';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	state $type = Tuple [LoreId, Int, Int];
	my @parts = split quotemeta Server::Config::PROTOCOL_SEPARATOR, $data;

	$type->assert_valid(\@parts);

	return [$parts[0], transport_floats_rev(@parts[1, 2])];
}

sub handle ($self, $player_id, $id, $data)
{
	$self->game_process->server->use_ability_check($player_id, $data->@*);

	return;
}

