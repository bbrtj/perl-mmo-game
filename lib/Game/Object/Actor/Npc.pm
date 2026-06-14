package Game::Object::Actor::Npc;

use My::Moose;

use header;

has param 'lore' => (
	lax_isa => InstanceOf ['Game::Lore::Npc'],
);

has param 'spawn' => (
	lax_isa => InstanceOf ['Game::Object::Map::Spawn'],
);

# TODO: NPC AI

