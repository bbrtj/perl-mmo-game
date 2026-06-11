package Game::Lore::Race;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'RACE';

has param 'stat_bonuses' => (
	isa => HashRef [Int],
);

