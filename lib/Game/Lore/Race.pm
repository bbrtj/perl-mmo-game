package Game::Lore::Race;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'race';

has param 'size_multiplier' => (
	isa => Num,
	default => 1,
);

has param 'base_stats' => (
	isa => HashRef [Int],
);

has param 'playable' => (
	isa => Bool,
);

