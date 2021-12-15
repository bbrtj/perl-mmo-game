package Game::Unit::Actor;

use Moo;
use DI;
use Game::Model::Player;
use Game::Model::Character;
use Game::Model::CharacterVariables;
use Types;

use header;

with 'Game::Unit';

has 'player' => (
	is => 'rw',
	isa => Types::Maybe [Types::InstanceOf ['Game::Model::Player']],
	predicate => 'is_player',
);

has 'npc' => (
	is => 'rw',

	# isa => Types::Maybe[Types::InstanceOf['Game::Model::Npc']],
	predicate => 'is_npc',
);

has 'character' => (
	is => 'rw',
	isa => Types::InstanceOf ['Game::Model::Character'],
);

has 'variables' => (
	is => 'rw',
	isa => Types::InstanceOf ['Game::Model::CharacterVariables'],
);

has 'cache' => (
	is => 'ro',
	isa => Types::HashRef,
	lazy => 1,
	default => sub ($self) {
		return DI->get('repo')->char_cache->load($self->character->id);
	},
);

sub set_cache_key ($self, $key, $value)
{
	my $cache = $self->cache;
	$cache->{$key} = $value;
	DI->get('repo')->char_cache->save($self->character->id, $cache);

	return;
}

