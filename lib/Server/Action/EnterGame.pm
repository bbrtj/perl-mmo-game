package Server::Action::EnterGame;

use My::Moose;
use Model;
use Resource::MapData;

use header;

extends 'Server::Action';

has injected 'units';
has injected 'lore_data';

use constant name => 'enter_game';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

sub validate ($self, $data)
{
	state $check = Types::Ulid;
	$check->assert_valid($data);
	return $data;
}

sub handle ($self, $session_id, $id, $data)
{
	my $session = $self->cache->load(PlayerSession => $session_id);
	my $success = 1;
	my $actor;
	my $player;

	try {
		$actor = $self->units->load_actor('player.id' => $data);
		$player = $actor->player;

		# check if that player belongs to the user in question
		$success = $player->user_id eq $session->user_id;
	}
	catch ($e) {
		$success = 0;
	}

	if ($success) {
		# TODO: check if any other session is logged in?
		# TODO: player might not be able to enter game if character is locked

		# TODO: start sending location data to the player
		# TODO: let game process know that it should add the player to its data
		$session->set_state($session->STATE_PLAYING);
		$session->set_location($actor->variables->location_id);
		$player->set_online(1);

		$self->models->save($player);
		$self->cache->save($session);

	}

	return $self->send_to(
		$session_id,
		$success,
		id => $id,
		refresh => 1
	);
}

