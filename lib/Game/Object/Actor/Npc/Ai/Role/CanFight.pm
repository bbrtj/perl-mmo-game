package Game::Object::Actor::Npc::Ai::Role::CanFight;

use My::Moose::Role;
use Game::Mechanics::Distance qw(calculate_distance);
use Game::Mechanics::Generic qw(calculate_angle_and_diagonal find_frontal_point);
use Game::Mechanics::Rng qw(rng random_choice);
use Math::Trig qw(pi);
use List::Util qw(max);

use header;

use constant ACCEPTABLE_DISTANCE_DIFF => 0.1;

requires qw(
	parent
);

has param 'follow_distance' => (
	lax_isa => Num,
	default => 0.1,
);

has param 'max_attack_distance' => (
	lax_isa => Num,
	default => 1,
);

sub fight ($self, $server, $npc_actor)
{
	my $aggro = $self->parent->aggro_map;
	return false unless $aggro->%*;

	my @xy = $npc_actor->variables->xy;

	my $max_aggro;
	my $max_aggro_value = '-inf';
	foreach my ($actor_id, $aggro_value) ($aggro->%*) {
		my $actor = $server->location->get_actor($actor_id);
		if (!$actor) {
			delete $aggro->{$actor_id};
			next;
		}

		my $distance = calculate_distance(@xy, $actor->variables->xy);
		my $this_aggro = $aggro_value / $distance**2;

		if ($this_aggro > $max_aggro_value) {
			$max_aggro = $actor;
			$max_aggro_value = $this_aggro;
		}
	}

	return false unless defined $max_aggro;

	# TODO: pathfinding
	my ($angle, $distance) = calculate_angle_and_diagonal(@xy, $max_aggro->variables->xy);
	my $stats = $npc_actor->stats;
	my $npc_size = $stats->size;
	my $follow_distance = $self->follow_distance + $npc_size;
	state $deviance_angle = pi / 3;

	# TODO: different behavior for ranged enemies
	if ($distance > $follow_distance + ACCEPTABLE_DISTANCE_DIFF || abs($angle - $stats->angle) > $deviance_angle) {
		my @point = find_frontal_point(
			@xy,
			$angle + rng() * $deviance_angle - $deviance_angle / 2,
			max($distance - $follow_distance, 0.001),    # make sure to walk towards the target
		);

		$server->set_movement($npc_actor->id, @point);
	}

	# TODO: use real abilities
	# TODO: npc should not spam abilities every second - make them take longer, or only use sometimes
	# TODO: npc should not damage other npcs
	if (!$stats->has_action && $distance < $self->max_attack_distance) {
		$server->use_ability($npc_actor->id, lore_id => 'abil.strike', x => 0, y => 0);
	}

	return true;
}

