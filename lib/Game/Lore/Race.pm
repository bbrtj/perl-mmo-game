package Game::Lore::Race;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'RACE';

has param 'base_stats' => (
	isa => HashRef [Int],
);

