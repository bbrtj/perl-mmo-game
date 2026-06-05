package Game::Object::Actor::Stats;

use My::Moose;
use Game::Config;
use Game::Mechanics::Character::Statistics;

use header;

has param 'parent' => (
	isa => Types::InstanceOf ['Unit::Actor'],
	weak_ref => 1,
);

# TODO: these are here because variables are tied to DB - consider using
# variables anyway or some runtime-only variables, to keep stats for cached
# statistics

has field 'movement' => (
	isa => Types::InstanceOf ['Game::Object::Movement'],
	writer => -hidden,
	clearer => 1,
);

# angle is needed because movement is optional
has field 'angle' => (
	writer => 1,
	default => 0,
);

has field 'action' => (

	# isa => InstanceOf['Game::Object::Action'],
	writer => 1,
	clearer => 1,
	predicate => 1,
);

has cached 'speed' => (
	writer => 1,
	default => sub { Game::Config->config->{base_speed} },    # TODO
);

# precalculated weapon damage
has cached 'weapon_damage' => (

	# isa => Types::PositiveNum,
	lazy => 1,
);

has cached 'weapon_hitbox' => (

	# isa => Types::Tuple[Types::PositiveNum, Types::Num],
	lazy => 1,
);

has cached 'max_health' => (

	# isa => Types::PositiveNum,
	lazy => 1,
);

has cached 'health_regeneration' => (

	# isa => Types::PositiveNum,
	lazy => 1,
);

has cached 'max_energy' => (

	# isa => Types::PositiveNum,
	lazy => 1,
);

has cached 'energy_regeneration' => (

	# isa => Types::PositiveNum,
	lazy => 1,
);

sub set_movement ($self, $movement)
{
	$self->_set_movement($movement);
	$self->set_angle($movement->angle);
	return;
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
	my $repo = DI->get('lore_data_repo');
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);
	my $class = $repo->load($self->parent->character->class_id);

	# TODO: adjust based on $level - 1
	# TODO: adjust based on stamina
	return $class->data->define->{base_health};
}

sub _build_health_regeneration ($self)
{
	my $repo = DI->get('lore_data_repo');
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);
	my $class = $repo->load($self->parent->character->class_id);

	# TODO: nasty hardcode
	return 0.5;
}

sub _build_max_energy ($self)
{
	my $repo = DI->get('lore_data_repo');
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);
	my $class = $repo->load($self->parent->character->class_id);

	# TODO: adjust based on $level - 1
	# TODO: adjust based on stamina
	return $class->data->define->{base_energy};
}

sub _build_energy_regeneration ($self)
{
	my $repo = DI->get('lore_data_repo');
	my $level = Game::Mechanics::Character::Statistics->get_current_level($self->parent->variables->experience);
	my $class = $repo->load($self->parent->character->class_id);

	# TODO: nasty hardcode
	return 0.1;
}

