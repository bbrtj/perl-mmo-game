package Game::Repository;

use Moo;
use Game::Common;
use Types;

use header;

our $VERSION = "0.001";

Game::Common->load_classes('Game::Repository', 'Repository/*.pm');

has 'char_cache' => (
	is => 'ro',
	isa => Types::ConsumerOf ['Game::Repository::Role::Resource'],
	default => sub { Game::Repository::CharCache->new },
);

has 'lore_data' => (
	is => 'ro',
	isa => Types::ConsumerOf ['Game::Repository::Role::Resource'],
	default => sub { Game::Repository::LoreData->new },
);

has 'schema' => (
	is => 'ro',
	isa => Types::ConsumerOf ['Game::Repository::Role::Resource'],
	default => sub { Game::Repository::Schema->new },
);

has 'actor_unit' => (
	is => 'ro',
	isa => Types::ConsumerOf ['Game::Repository::Role::Resource'],
	default => sub { Game::Repository::ActorUnit->new },
);

has 'battle_unit' => (
	is => 'ro',
	isa => Types::ConsumerOf ['Game::Repository::Role::Resource'],
	default => sub { Game::Repository::BattleUnit->new },
);

