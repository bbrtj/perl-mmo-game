package Game::Object::Actor::Npc;

use My::Moose;
use all 'Game::Object::Actor::Npc::Ai';

use header;

use constant AGGRO_DEGRADATION => 0.8;
use constant MIN_AGGRO => 0.01;

has param 'spawn' => (
	lax_isa => InstanceOf ['Game::Object::Map::Spawn'],
	'handles->' => {
		'lore' => 'lore',
	},
);

has field 'ai' => (
	lax_isa => Maybe [InstanceOf ['Game::Object::Actor::Npc::Ai']],
	lazy => 1,
);

has field 'aggro_map' => (
	lax_isa => HashRef,
	default => sub { {} },
);

has cached 'race' => (
	lax_isa => InstanceOf ['Game::Lore::Race'],
	lazy => 1,
);

# NOTE: NPCs should have just one race entry, otherwise it's configuration error
sub _build_race ($self)
{
	return $self->lore->races->[0];
}

sub _build_ai ($self)
{
	my $lore = $self->lore;
	return undef unless $lore->has_ai;

	return $lore->ai_class->new($lore->ai_args->%*, parent => $self);
}

sub add_aggro ($self, $actor, $value)
{
	$self->aggro_map->{$actor->id} += $value;
	return;
}

sub reduce_aggro ($self)
{
	foreach my ($actor_id, $aggro_value) ($self->aggro_map->%*) {
		$aggro_value = $aggro_value * AGGRO_DEGRADATION;
		delete $self->aggro_map->{$actor_id}
			if $aggro_value < MIN_AGGRO;
	}

	return;
}

