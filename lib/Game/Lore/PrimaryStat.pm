package Game::Lore::PrimaryStat;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'pstat';

has param 'affects' => (
	isa => HashRef [Num],
);

