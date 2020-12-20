package Game::Unit::Actor;

use header;
use Moo;
use Game::Model::Player;
use Game::Model::Character;
use Game::Model::CharacterVariables;
use Game::Types qw(Maybe InstanceOf HashRef);

no header;

with 'Game::Unit';

has 'player' => (
	is => 'rw',
	isa => Maybe [InstanceOf ['Game::Model::Player']],
	predicate => is_player,
);

has 'npc' => (
	is => 'rw',

	# isa => Maybe[InstanceOf['Game::Model::Npc']],
	predicate => is_npc,
);

has 'character' => (
	is => 'rw',
	isa => InstanceOf ['Game::Model::Character'],
);

has 'variables' => (
	is => 'rw',
	isa => InstanceOf ['Game::Model::CharacterVariables'],
);

has 'precalculated' => (
	is => 'rw',
	isa => HashRef,
);

1;
