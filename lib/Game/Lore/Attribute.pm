package Game::Lore::Attribute;

use My::Moose;

use header;

extends 'Game::Lore';

use constant prefix => 'ATTR';

has extended 'parent' => (
	isa => InstanceOf ['Game::Lore::Attribute'],
);

