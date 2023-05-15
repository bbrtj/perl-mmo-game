package Game::Object::Actor::Stats;

use My::Moose;
use Game::Config;

use header;

has param 'parent' => (
	isa => Types::InstanceOf['Unit::Actor'],
	weak_ref => 1,
);

has field 'movement' => (
	isa => Types::InstanceOf['Game::Object::Movement'],
	writer => -hidden,
	clearer => 1,
);

# angle is needed because movement is optional
has field 'angle' => (
	writer => 1,
	default => 0,
);

has field 'speed' => (
	writer => 1,
	default => Game::Config->config->{base_speed}, # TODO
);

has field 'last_action' => (
	# isa => Types::PositiveOrZeroNum,
	writer => -hidden,
	default => 0,
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

sub set_movement ($self, $movement)
{
	$self->_set_movement($movement);
	$self->set_angle($movement->angle);
	return;
}

sub action_performed ($self)
{
	my $time = server_time;

	return !!0
		unless $time - $self->last_action >= Game::Config->config->{action_cooldown};

	$self->_set_last_action($time);
	return !!1;
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

