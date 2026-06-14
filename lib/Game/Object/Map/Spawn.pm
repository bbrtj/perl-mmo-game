package Game::Object::Map::Spawn;

use My::Moose;

use header;

has param 'lore' => (
	isa => InstanceOf ['Game::Lore::Npc'],
);

has field 'next_respawn' => (
	lax_isa => PositiveOrZeroNum,
	writer => -hidden,
	default => 0,
);

with qw(
	Game::Object::Role::HasPosition
);

sub set_next_respawn ($self, $time = server_time)
{
	$self->_set_next_respawn($time + $self->lore->respawn_time);
	return;
}

sub should_respawn ($self, $time = server_time)
{
	return $time >= $self->next_respawn;
}

