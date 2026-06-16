package Game::Lore::Class;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'class';

has param 'health_multiplier' => (
	isa => Num,
	default => 1,
);

has param 'speed_multiplier' => (
	isa => Num,
	default => 1,
);

has param 'energy_multiplier' => (
	isa => Num,
	default => 1,
);

has param 'stat_bonuses' => (
	isa => HashRef [Int],
);

