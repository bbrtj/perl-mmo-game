package Game::Server::Role::Combat;

use My::Moose::Role;

use all 'Game::Mechanics';
use Resource::ActorEvent;

use header;

requires qw(
	find_in_radius
	send_to_players
	get_discovered_by
);

sub use_ability ($self, $actor_id, %options)
{
	# TODO: check and use proper ability from $options{ability}

	my $actor = $self->location->get_actor($actor_id);
	my $stats = $actor->stats;

	# check and set global cooldown
	return unless $stats->action_performed;

	my ($x, $y) = ($actor->variables->pos_x, $actor->variables->pos_y);
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
			Resource::ActorEvent->new(subject => $affected, event_source => $actor_id, health_change => -$damage)
		);
	}

	# TODO: other players in range should know that the ability has taken place, so they can animate it
	# the hard part: use players which have discovered each player - possibly different list for each affected player (for big AOEs)

	return;
}

