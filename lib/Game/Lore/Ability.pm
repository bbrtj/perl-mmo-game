package Game::Lore::Ability;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'abil';

has param 'speed_multiplier' => (
	isa => PositiveNum,
	default => 1,
);

has param 'damage_multiplier' => (
	isa => PositiveNum,
	default => 1,
);

has param 'magical' => (
	isa => Bool,
	default => false,
);

has param 'energy_cost' => (
	isa => PositiveOrZeroNum,
	default => 0,
);

has option 'projectile' => (
	isa => Dict [
		speed => PositiveNum,
		range => PositiveNum,
		inaccuracy => PositiveOrZeroNum,
		collision => PositiveNum,
	],
);

