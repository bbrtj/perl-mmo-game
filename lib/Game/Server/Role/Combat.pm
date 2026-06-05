package Game::Server::Role::Combat;

use My::Moose::Role;

use all 'Game::Mechanics';
use all 'Game::Object';
use Resource::ActorEvent;

use header;

requires qw(
	lore_data_repo
	find_in_radius
	send_to_players
	get_discovered_by
	enqueue_action
);

sub use_ability ($self, $actor_id, %options)
{
	my $actor = $self->location->get_actor($actor_id);
	my $stats = $actor->stats;

	# do nothing if action is in progress already
	return if $stats->has_action;

	my $ability = $self->lore_data_repo->load($options{lore_id});
	my $action = Game::Object::Action::Ability->new(
		%options,
		actor => $actor,
		duration => Game::Config->config->{base_action_speed} * $ability->data->speed_multiplier,
	);

	$stats->set_action($action);
	$self->enqueue_action($action);

	$self->send_to_players(
		[$actor_id, $self->get_discovered_by($actor_id)],
		Resource::ActorAction->new(subject => $action)
	);

	return;
}

sub use_ability_done ($self, $object)
{
	my $actor = $object->actor;
	my $ability = $self->lore_data_repo->load($object->lore_id);

	# TODO: use x and y from object is applicable
	# TODO: check and use proper ability from $object->lore_id
	my $stats = $actor->stats;
	$stats->clear_action;

	my ($radius, $distance) = $stats->weapon_hitbox->@*;

	my @found = grep { $_ != $actor } Game::Mechanics::Distance->find_actors_in_range(
		$self,
		Game::Mechanics::Generic->find_frontal_point($actor->variables->xy, $stats->angle, $distance),
		$radius
	);

	# TODO: calculate ability damage
	my $damage = $stats->weapon_damage;
	Game::Mechanics::Character::Damage->deal_damage($ability->data->attributes, $damage, @found);

	# TODO: not always all targets will be affected (ability target limit)
	foreach my $affected (@found) {
		$self->send_to_players(
			[$affected->id, $self->get_discovered_by($affected->id)],
			Resource::ActorEvent->new(subject => $affected, event_source => $actor->id, health_change => -$damage)
		);
	}

	# TODO: other players in range should know that the ability has taken
	# place, so they can animate it - even if no players were affected

	return;
}

