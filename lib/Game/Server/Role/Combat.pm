package Game::Server::Role::Combat;

use My::Moose::Role;

use Game::Mechanics::Combat qw(deal_damage is_friendly);
use Game::Mechanics::Generic qw(find_frontal_point);
use Game::Checks::Combat qw(can_use_ability);
use all 'Game::Object';
use Resource::ActorEvent;

use header;

requires qw(
	location
	lore_data_repo
	actors_collision
	send_to_players
	get_discovered_by
	enqueue_action
	spawn_projectile
);

sub use_ability_check ($self, $actor_id, $lore_id, $x, $y)
{
	my $actor = $self->location->get_actor($actor_id);
	my $lore = $self->lore_data_repo->maybe_load($lore_id);
	can_use_ability($actor, $lore, $x, $y);

	$self->use_ability($actor, $lore, $x, $y);
	return;
}

sub use_ability ($self, $actor, $ability, $x, $y)
{
	my $action = Game::Object::Action::Ability->new(
		lore => $ability,
		actor => $actor,
		x => $x,
		y => $y,
		duration => Game::Config->base_action_speed * $ability->speed_multiplier,
	);

	$actor->stats->set_action($action);
	$self->enqueue_action($action);

	$self->send_to_players(
		[$actor->id, $self->get_discovered_by($actor->id)],
		Resource::ActorAction->new(subject => $action)
	);

	return;
}

sub _apply_damage_effect ($self, $effect, $x, $y)
{
	my $actor = $effect->actor;
	my $stats = $actor->stats;

	# TODO: calculate ability damage, radius
	my $damage = $stats->weapon_damage * $effect->lore->damage_multiplier;
	my $radius = $stats->weapon_hitbox->[0];

	my @found = grep { !is_friendly($actor, $_) } $self->actors_collision($x, $y, $radius)->@*;
	deal_damage($actor, $effect->lore->attributes, $damage, @found);

	# TODO: not always all targets will be affected (ability target limit)
	foreach my $affected (@found) {
		if ($affected->variables->dead) {
			$self->signal(actor_died => $affected);
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

