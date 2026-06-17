package Game::Object::Actor::Npc;

use My::Moose;
use Utils;
use all 'Game::Object::Actor::Npc::Ai';

use header;

has param 'lore' => (
	lax_isa => InstanceOf ['Game::Lore::Npc'],
);

has param 'spawn' => (
	lax_isa => InstanceOf ['Game::Object::Map::Spawn'],
);

has field 'ai' => (
	lax_isa => Maybe [InstanceOf ['Game::Object::Actor::Npc::Ai']],
	lazy => 1,
);

has field 'race' => (
	lax_isa => InstanceOf ['Game::Lore::Race'],
	lazy => 1,
);

has field 'aggro_map' => (
	lax_isa => HashRef,
	default => sub { {} },
);

# NOTE: NPCs should have just one race
sub _build_race ($self)
{
	return $self->lore->races->[0];
}

sub _build_ai ($self)
{
	return unless $self->lore->has_ai;

	my $class = 'Game::Object::Actor::Npc::Ai::' . Utils->pascal_case($self->lore->ai);
	return $class->new($self->lore->ai_args->%*, parent => $self);
}

sub add_aggro ($self, $actor, $value)
{
	$self->aggro_map->{$actor->id} += $value;
	return;
}

