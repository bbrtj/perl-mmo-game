package Game::Server::Role::Combat;

use My::Moose::Role;

use all 'Game::Mechanics';
use all 'Game::Object';
use Resource::ActorEvent;

use header;

requires qw(
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

	# TODO: check and use proper ability speed from $options{ability}
	my $action = Game::Object::Action->new(
		method => 'use_ability_done',
		args => [$actor, %options],
		duration => Game::Config->config->{base_action_speed},
	);

	$stats->set_action($action);
	$self->enqueue_action($action);

	# TODO: notify the client the action is taking place?
	return;
}

sub use_ability_done ($self, $actor, %options)
{
	# TODO: check and use proper ability from $options{ability}
	my $stats = $actor->stats;
	$stats->clear_action;

	my ($x, $y) = $actor->variables->xy;
	my ($radius, $distance) = $stats->weapon_hitbox->@*;

	my @found = grep { $_ != $actor } Game::Mechanics::Distance->find_actors_in_range(
		$self,
		Game::Mechanics::Generic->find_frontal_point($x, $y, $stats->angle, $distance),
		$radius
	);

	# TODO: attribute
	# TODO: calculate ability damage
	my $damage = $stats->weapon_damage;
	Game::Mechanics::Character::Damage->deal_damage('todo', $damage, @found);

	foreach my $affected (@found) {
		$self->send_to_players(
			[$affected->id, $self->get_discovered_by($affected->id)],
			Resource::ActorEvent->new(subject => $affected, event_source => $actor->id, health_change => -$damage)
		);
	}

	# TODO: other players in range should know that the ability has taken place, so they can animate it
	# the hard part: use players which have discovered each player - possibly different list for each affected player (for big AOEs)

	return;
}

