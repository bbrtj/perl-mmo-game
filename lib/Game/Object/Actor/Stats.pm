use experimental 'class';

class Game::Object::Actor::Stats;

use Game::Config;
use Game::Mechanics::Character::Statistics qw(:all);

use header;

field $parent :param;

field $level :reader;
field $stats :reader;
field $speed :reader;
field $size :reader;
field $weapon_damage :reader;
field $weapon_hitbox :reader;
field $max_health :reader;
field $health_regeneration :reader;
field $max_energy :reader;
field $energy_regeneration :reader;
field $luck_effect :reader;

ADJUST {
	weaken $parent;
	state $secondary = DI->get('lore_data_repo')->load_all_named('Game::Lore::SecondaryStat');

	my $char = $parent->character;
	my $class = $char->class;
	my $race = $char->race;

	# NOTE: npc gets experience set to the right number upon spawning
	$level = get_current_level($parent->variables->experience);

	$stats = {};

	foreach my ($stat, $value) ($race->base_stats->%*) {
		$stats->{$stat} = $value;
	}

	foreach my $stat (keys $secondary->%*) {
		$stats->{$stat} = 0;
	}

	foreach my ($stat, $value) ($class->stat_bonuses->%*) {
		$stats->{$stat} += exists $secondary->{$stat}
			? int($self->level * $value)
			: $value
			;
	}

	$speed = get_speed(
		$class,
		$stats
	);

	$size = get_size(
		$race,
		$stats,
	);

	# TODO calculate from equipment and other stats
	$weapon_damage = 5;

	# TODO calculate from equipment and other stats
	# [radius, distance from character]
	$weapon_hitbox = [0.25, 0.2];

	$max_health = get_max_health(
		$class,
		$stats,
	);

	$health_regeneration = get_health_regen(
		$class,
		$stats,
	);

	$max_energy = get_max_energy(
		$class,
		$stats,
	);

	$energy_regeneration = get_energy_regen(
		$class,
		$stats,
	);

	$luck_effect = get_luck_effect(
		$stats,
	);
}

