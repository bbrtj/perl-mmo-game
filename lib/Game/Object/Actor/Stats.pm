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

has cached 'speed' => (
	writer => 1,
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

sub _build_speed ($self)
{
	# TODO calculate from stats
	return Game::Config->base_speed;
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
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);

	# TODO: adjust based on $level - 1
	# TODO: adjust based on stamina
	return Game::Config->base_health;
}

sub _build_health_regeneration ($self)
{
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);

	# TODO: nasty hardcode
	return 0.5;
}

sub _build_max_energy ($self)
{
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);

	# TODO: adjust based on $level - 1
	# TODO: adjust based on stamina
	return Game::Config->base_energy;
}

sub _build_energy_regeneration ($self)
{
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);

	# TODO: nasty hardcode
	return 0.1;
}

sub _build_size ($self)
{
	# TODO: size will be affected by race and constitution
	return Game::Config->base_size;
}

