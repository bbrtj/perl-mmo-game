package Server::Command::EnterGame;

use My::Moose;
use Model;
use Resource::MapData;

use header;

extends 'Server::Command';

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
	state $repo = DI->get('units');
	state $lore_data = DI->get('lore_data');

	my $session = $self->cache->load(PlayerSession => $session_id);
	my $success = 1;
	my $actor;

	try {
		$actor = $repo->load_actor('player.id' => $data);

		# double check if that player belongs to the user in question
		$success = $actor->player->user_id eq $session->user_id;
	}
	catch ($e) {
		$success = 0;
	}

	if ($success) {

		# TODO: start sending location data to the player
		# TODO: switch player online status in the DB
		# TODO: let game process know that it should add the player to its data
		# TODO: player might not be able to enter game if character is locked
		$session->set_state($session->STATE_PLAYING);
		$session->set_location($actor->variables->location_id);

		$self->cache->save($session);
	}

	return $self->send_to(
		$session_id,
		$success,
		id => $id,
		refresh => 1
	);
}

