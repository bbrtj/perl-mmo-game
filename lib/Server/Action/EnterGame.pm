package Server::Action::EnterGame;

use My::Moose;
use all 'Model';
use X::Pub;
use Resource::Success;

use header;

extends 'Server::Action';

has injected 'units_repo';

use constant name => 'enter_game';
use constant required_state => Model::PlayerSession->STATE_LOGGED_IN;

sub validate ($self, $data)
{
	ULID->assert_valid($data);
	return $data;
}

sub handle ($self, $session_id, $id, $player_id)
{
	my $session = $self->cache_repo->load(PlayerSession => $session_id);
	my $success = true;
	my $actor;
	my $player;

	try {
		$actor = $self->units_repo->load_actor('player.id' => $player_id);
		$player = $actor->player;

		# check if that player belongs to the user in question
		$success &&= $player->user_id eq $session->user_id;

		# TODO: check if any other session is logged in?
		# TODO: player might not be able to enter game if character is locked

	}
	catch ($e) {
		$success = false;
	}

	X::Pub->raise
		unless $success;

	$self->data_bus->dispatch(
		$actor->variables->location_id,
		'player_has_entered_game',
		$player->id,
		$session->id
	);

	$self->send_to(
		$session_id,
		Resource::Success->new,
		id => $id,
	);

	return;
}

