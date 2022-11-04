package Server::Event::PlayerHasEnteredLocation;

use My::Moose;
use Resource::LocationData;

use header;

extends 'Server::Event';

has injected 'units';

use constant name => 'player_has_entered_location';

sub update_session ($self, $session)
{
	$session->set_location_id($self->game_process->location_data->location->id);
	# TODO: update session player?

	return;
}

sub update_actor ($self, $actor)
{
	$actor->variables->set_location_id($self->game_process->location_data->location->id);

	# TODO: set x/y
	return;
}

sub handle ($self, $session_id, $player_id)
{
	my $session = $self->cache->load(PlayerSession => $session_id);
	$self->update_session($session);
	$self->cache->save($session);

	my $actor = $self->units->load_actor('player.id' => $player_id);
	$self->update_actor($actor);
	$self->units->update($actor);

	$self->game_process->location_data->add_actor($actor);

	# TODO: how to treat session?
	# TODO: handle player / session

	return $self->send_to(
		$session_id,
		Resource::LocationData->new($self->game_process->location_data->location),
		refresh => 1
	);
}

