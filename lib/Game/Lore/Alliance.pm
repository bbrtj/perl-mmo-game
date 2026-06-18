package Game::Lore::Alliance;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'alli';

has param 'playable' => (
	isa => Bool,
	default => false,
);

