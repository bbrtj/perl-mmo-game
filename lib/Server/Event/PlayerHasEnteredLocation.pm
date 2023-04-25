package Server::Event::PlayerHasEnteredLocation;

use My::Moose;
use Resource::LocationData;

use header;

extends 'Server::Event';

has injected 'units_repo';

use constant name => 'player_has_entered_location';

sub update_session ($self, $session, $actor)
{
	$session->set_location_id($self->game_process->location_id);

	return;
}

sub update_actor ($self, $session, $actor)
{
	$actor->variables->set_location_id($self->game_process->location_id);

	# TODO: set x/y of the point in which actor has appeared (when they moved between locations)
	return;
}

sub handle ($self, $player_id, $session_id)
{
	my $actor = $self->units_repo->load_actor('player.id' => $player_id);
	my $session = $self->cache_repo->load(PlayerSession => $session_id);

	$self->update_actor($session, $actor);
	$self->update_session($session, $actor);

	$self->units_repo->update($actor);
	$self->cache_repo->save($session);

	$self->server->location->add_actor($actor);

	$self->send_to(
		$session_id,
		Resource::LocationData->new(subject => $self->server->location->lore),
		refresh => 1
	);

	# fun way to update actor on their own position
	$self->server->signal_actor_appeared($actor, $actor);

	return;
}

