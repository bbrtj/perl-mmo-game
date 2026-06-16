package Game::Object::Actor::Stats;

use My::Moose;
use Game::Config;
use Game::Mechanics::Character::Statistics;

use header;

has param 'parent' => (
	isa => InstanceOf ['Unit::Actor'],
	weak_ref => 1,
);

# TODO: these are here because variables are tied to DB - consider using
# variables anyway or some runtime-only variables, to keep stats for cached
# statistics

has field 'movement' => (
	isa => InstanceOf ['Game::Object::Movement'],
	writer => -hidden,
	clearer => 1,
);

# angle is needed because movement is optional
has field 'angle' => (
	writer => 1,
	default => 0,
);

has field 'action' => (
	lax_isa => InstanceOf ['Game::Object::Action'],
	writer => 1,
	clearer => 1,
	predicate => 1,
);

has cached 'level' => (
	lax_isa => Int,
	lazy => 1,
);

has cached 'stats' => (
	lax_isa => HashRef [Int],
	lazy => 1,
);

has cached 'speed' => (
	lax_isa => PositiveOrZeroNum,
	lazy => 1,
);

# precalculated weapon damage
has cached 'weapon_damage' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

has cached 'weapon_hitbox' => (
	lax_isa => Tuple [PositiveNum, Num],
	lazy => 1,
);

has cached 'max_health' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

has cached 'health_regeneration' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

has cached 'max_energy' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

has cached 'energy_regeneration' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

has cached 'size' => (
	lax_isa => PositiveNum,
	lazy => 1,
);

sub set_movement ($self, $movement)
{
	$self->_set_movement($movement);
	$self->set_angle($movement->angle);
	return;
}

# NOTE: npc gets experience set to the right number upon spawning
sub _build_level ($self)
{
	return Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);
}

sub _build_stats ($self)
{
	state $secondary = DI->get('lore_data_repo')->load_all_named('Game::Lore::SecondaryStat');
	my $char = $self->parent->character;
	my $level = $self->level;
	my %calculated;

	foreach my ($stat, $value) ($char->race->base_stats->%*) {
		$calculated{$stat} = $value;
	}

	foreach my $stat (keys $secondary->%*) {
		$calculated{$stat} = 0;
	}

	foreach my ($stat, $value) ($char->class->stat_bonuses->%*) {
		$calculated{$stat} += exists $secondary->{$stat}
			? int($self->level * $value)
			: $value
			;
	}

	return \%calculated;
}

sub _build_speed ($self)
{
	return Game::Mechanics::Character::Statistics->get_speed(
		$self->stats
	);
}

sub _build_weapon_damage ($self)
{
	# TODO calculate from equipment and other stats
	return 5;
}

sub _build_weapon_hitbox ($self)
{
	# TODO calculate from equipment and other stats
	# [radius, distance from character]
	return [0.25, 0.2];
}

sub _build_max_health ($self)
{
	return Game::Mechanics::Character::Statistics->get_max_health(
		$self->parent->character->class,
		$self->stats,
	);
}

sub _build_health_regeneration ($self)
{
	return Game::Mechanics::Character::Statistics->get_health_regen(
		$self->parent->character->class,
		$self->stats,
	);
}

sub _build_max_energy ($self)
{
	return Game::Mechanics::Character::Statistics->get_max_energy(
		$self->parent->character->class,
		$self->stats,
	);
}

sub _build_energy_regeneration ($self)
{
	return Game::Mechanics::Character::Statistics->get_energy_regen(
		$self->parent->character->class,
		$self->stats,
	);
}

sub _build_size ($self)
{
	my $char = $self->parent->character;
	return Game::Mechanics::Character::Statistics->get_size(
		$char->race,
		$char->class,
		$self->stats,
	);
}

