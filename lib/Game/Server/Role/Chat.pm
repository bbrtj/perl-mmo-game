package Game::Server::Role::Chat;

use My::Moose::Role;
use Game::Config;

use all 'Resource';

use header;

requires qw(
	get_discovered_by
	find_in_radius
);

sub _chat_send ($self, $actor, $resource, $recipients)
{
	# TODO: check if the actor is muted, throw X::Pub otherwise
	$self->send_to_players(
		$recipients,
		$resource,
	);

	return;
}

sub chat_say ($self, $actor_id, $message)
{
	my $actor = $self->location->get_actor($actor_id);

	$self->_chat_send(
		$actor,
		Resource::Chat->new(subject => $actor, message => $message),
		[$actor_id, $self->get_discovered_by($actor_id)],
	);

	return;
}

sub chat_yell ($self, $actor_id, $message)
{
	my $actor = $self->location->get_actor($actor_id);

	$self->_chat_send(
		$actor,
		Resource::Chat->new(subject => $actor, message => $message, chat_type => Resource::Chat->YELL),
		$self->find_in_radius(
			$actor->variables->pos_x,
			$actor->variables->pos_y,
			Game::Config->config->{yell_radius}
		),
	);

	return;
}

sub chat_whisper ($self, $actor_id, $target_actor, $message)
{
	my $actor = $self->location->get_actor($actor_id);

	# TODO: find target by name (on all servers, not just this one)
	# TODO: send message to target
	# TODO: send message to source actor (with prefix like "(to NAME)")
	# TODO: notify if not found by name
	$self->_chat_send(
		$actor,
		Resource::Chat->new(subject => $actor, message => "tried to message $target_actor with: $message", chat_type => Resource::Chat->WHISPER),
		[$actor_id]
	);

	return;
}

