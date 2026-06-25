package Server::Action::Move;

use My::Moose;
use all 'Model';
use Server::Config;
use Utils qw(transport_floats_rev);

use header;

extends 'Server::GameAction';

use constant name => 'move';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	state $type = Tuple [Int, Int];
	my @parts = split quotemeta Server::Config::PROTOCOL_SEPARATOR, $data;

	$type->assert_valid(\@parts);

	return [transport_floats_rev @parts];
}

sub handle ($self, $player_id, $id, $position)
{
	$self->game_process->server->set_movement_check($player_id, $position->@*);

	return;
}

