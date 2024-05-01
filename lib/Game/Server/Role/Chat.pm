package Game::Server::Role::Chat;

use My::Moose::Role;
use Game::Config;

use all 'Resource';

use header;

requires qw(
	get_discovered_by
	find_in_radius
	cache_repo
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

sub _chat_send_session ($self, $actor, $resource, $recipients)
{
	# TODO: check if the actor is muted, throw X::Pub otherwise
	$self->process->send_to_all(
		$resource,
		sessions => $recipients,
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

sub chat_private ($self, $actor_id, $target_actor, $message)
{
	my $actor = $self->location->get_actor($actor_id);

	try {
		my $other_session_lookup = $self->cache_repo->load(PlayerSessionLookup => $target_actor);

		$self->_chat_send(
			$actor,
			Resource::Chat->new(
				subject => $actor,
				sent_to => $other_session_lookup->id,
				message => $message,
				chat_type => Resource::Chat->PRIVATE
			),
			[$actor_id]
		);

		$self->_chat_send_session(
			$actor,
			Resource::Chat->new(subject => $actor, message => $message, chat_type => Resource::Chat->PRIVATE),
			[$other_session_lookup->session_id]
		);
	}
	catch ($e) {

		# TODO: notify if not found by name
		die $e;
	}

	return;
}

