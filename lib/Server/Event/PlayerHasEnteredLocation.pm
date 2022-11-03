package Server::Event::PlayerHasEnteredLocation;

use My::Moose;

use header;

extends 'Server::Event';

has injected 'units';

use constant name => 'player_has_entered_location';

sub update_session ($self, $session)
{
	$session->set_location($self->game_process->location_data->location->id);

	return;
}

sub update_actor ($self, $actor)
{
	$actor->variables->set_location($self->game_process->location_data->location->id);

	# TODO: set x/y
}

sub handle ($self, $session_id, $player_id)
{
	# TODO: start sending location data to the player

	my $session = $self->cache->load(PlayerSession => $session_id);
	$self->update_session($session);
	$self->cache->save($session);

	my $actor = $self->units->load_actor('player.id' => $player_id);
	$self->update_actor($actor);
	$self->units->save($actor);

	$self->game_process->add_actor($actor);

	# TODO: how to treat session?

	return $self->send_to(
		$session_id,
		undef,
		refresh => 1
	);
}

