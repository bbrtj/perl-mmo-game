package Game::Lore::Weapon;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'weap';

has param 'both_hands' => (
	isa => Bool,
);

has param 'damage_min' => (
	isa => PositiveOrZeroNum,
);

has param 'damage_max' => (
	isa => PositiveOrZeroNum,
);

