package Game::Object::Actor::Npc;

use My::Moose;

use header;

has param 'lore' => (
	lax_isa => InstanceOf ['Game::Lore::Npc'],
);

has param 'spawn' => (
	lax_isa => InstanceOf ['Game::Object::Map::Spawn'],
);

has field 'race' => (
	lax_isa => InstanceOf ['Game::Lore::Race'],
	builder => 1,
);

# NOTE: NPCs should have just one race
sub _build_race ($self)
{
	return $self->lore->races->[0];
}

# TODO: NPC AI

