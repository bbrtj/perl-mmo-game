package Game::Server::Role::Combat;

use My::Moose::Role;

use Game::Mechanics::Distance qw(find_actors_in_range);
use Game::Mechanics::Combat qw(deal_damage is_friendly);
use Game::Mechanics::Generic qw(find_frontal_point);
use all 'Game::Object';
use Resource::ActorEvent;

use header;

requires qw(
	location
	lore_data_repo
	find_in_radius
	send_to_players
	get_discovered_by
	enqueue_action
	spawn_projectile
);

sub use_ability ($self, $actor_id, %options)
{
	my $actor = $self->location->get_actor($actor_id);
	my $stats = $actor->stats;

	# do nothing if action is in progress already
	return if $stats->has_action;

	my $ability = $self->lore_data_repo->load(delete $options{lore_id});
	my $action = Game::Object::Action::Ability->new(
		%options,
		lore => $ability,
		actor => $actor,
		duration => Game::Config->base_action_speed * $ability->speed_multiplier,
	);

	$stats->set_action($action);
	$self->enqueue_action($action);

	$self->send_to_players(
		[$actor_id, $self->get_discovered_by($actor_id)],
		Resource::ActorAction->new(subject => $action)
	);

	return;
}

sub _apply_damage_effect ($self, $effect, $x, $y)
{
	my $actor = $effect->actor;
	my $stats = $actor->stats;

	# TODO: calculate ability damage, radius
	my $damage = $stats->weapon_damage;
	my $radius = $stats->weapon_hitbox->[0];

	my @found = grep { !is_friendly($actor, $_) } find_actors_in_range($self, $x, $y, $radius);
	deal_damage($actor, $effect->lore->attributes, $damage, @found);

	# TODO: not always all targets will be affected (ability target limit)
	foreach my $affected (@found) {
		if ($affected->variables->health <= 0) {
			$self->signal_actor_died($affected);
		}

		$self->send_to_players(
			[$affected->id, $self->get_discovered_by($affected->id)],
			Resource::ActorEvent->new(
				subject => $affected,
				event_source => $actor->id,
				health_change => -$damage
			)
		);
	}

	return;
}

sub use_ability_done ($self, $action)
{
	my $actor = $action->actor;
	my $ability = $action->lore;

	# TODO: take resources required by the ability (energy? arrows?)
	my $stats = $actor->stats;
	$stats->clear_action;

	my $effect = Game::Object::Effect::Damage->new(
		actor => $action->actor,
		lore => $ability,
	);

	# TODO: use x and y from object if applicable
	if ($ability->projectile) {

		# projectile attack
		$self->spawn_projectile($effect, $action->xy);
	}
	else {
		# frontal attack
		$self->apply_effect(
			$effect,
			find_frontal_point($actor->variables->xy, $stats->angle, $stats->weapon_hitbox->[1]),
		);
	}

	# TODO: other players in range should know that the ability has taken
	# place, so they can animate it - even if no players were affected

	return;
}

