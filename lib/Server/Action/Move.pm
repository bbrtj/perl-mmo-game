package Server::Action::Move;

use My::Moose;
use all 'Model';
use Server::Config;
use Game::Mechanics::Check::Map;

use header;

extends 'Server::GameAction';

has injected 'units_repo';

use constant name => 'move';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	state $type = Types::Tuple[Types::PositiveNum, Types::PositiveNum];
	my $parts = [split quotemeta Server::Config::PROTOCOL_SEPARATOR, $data];

	$type->assert_valid($parts);

	return $parts;
}

before handle => sub ($self, $player_id, $id, $position) {
	Game::Mechanics::Check::Map->can_move_to(
		$self->server->map,
		$self->server->get_player($player_id)->variables->xy,
		$position
	)->assert_valid;
};

sub handle ($self, $player_id, $id, $position)
{
	my $movement = $self->game_process->server->set_movement($player_id, $position->@*);

	return $self->send_to_player(
		$player_id,
		Resource::Movement->new($movement),
		id => $id,
	);
}

