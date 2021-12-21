package Game::Unit::Actor;

use Moo;
use DI;
use Model::Player;
use Model::Character;
use Model::CharacterVariables;
use Types;

use header;

with 'Game::Unit';

has 'player' => (
	is => 'rw',
	isa => Types::Maybe [Types::InstanceOf ['Model::Player']],
	predicate => 'is_player',
);

has 'npc' => (
	is => 'rw',

	# isa => Types::Maybe[Types::InstanceOf['Model::Npc']],
	predicate => 'is_npc',
);

has 'character' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Character'],
);

has 'variables' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::CharacterVariables'],
);

has 'cache' => (
	is => 'ro',
	isa => Types::HashRef,
	lazy => 1,
	default => sub ($self) {
		return DI->get('char_cache')->load($self->character->id);
	},
);

sub set_cache_key ($self, $key, $value)
{
	my $cache = $self->cache;
	$cache->{$key} = $value;
	DI->get('char_cache')->save($self->character->id, $cache);

	return;
}

