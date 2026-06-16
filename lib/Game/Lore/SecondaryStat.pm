package Game::Lore::SecondaryStat;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'sstat';

has param 'value' => (
	isa => Num,
);

