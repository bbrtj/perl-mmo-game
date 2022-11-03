package Server::Action::EnterGame;

use My::Moose;
use Model;
use Resource::MapData;

use header;

extends 'Server::Action';

has injected 'units';

use constant name => 'enter_game';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

sub validate ($self, $data)
{
	state $check = Types::Ulid;
	$check->assert_valid($data);
	return $data;
}

sub handle ($self, $session_id, $id, $player_id)
{
	my $session = $self->cache->load(PlayerSession => $session_id);
	my $success = 1;
	my $actor;
	my $player;

	try {
		$actor = $self->units->load_actor('player.id' => $player_id);
		$player = $actor->player;

		# check if that player belongs to the user in question
		$success = $player->user_id eq $session->user_id;

		# TODO: check if any other session is logged in?
		# TODO: player might not be able to enter game if character is locked

	}
	catch ($e) {
		$success = 0;
	}

	if ($success) {
		$self->data_bus->dispatch($actor->variables->location_id, 'player_has_entered_game', $session->id, $player->id);
	}

	return $self->send_to(
		$session_id,
		$success,
		id => $id,
	);
}
