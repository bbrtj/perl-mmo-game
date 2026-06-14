package Game::Lore::Npc;

use My::Moose;

use header;

extends 'Game::Lore::Class';

use constant prefix => 'NPC';

has param 'respawn_time' => (
	isa => PositiveInt,
);

