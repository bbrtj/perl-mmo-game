package Game::Repository;

use header;
use Moo;
use Game::Common;
use Game::Common::Container qw(add_to_container);

no header;

extends 'Game::RepositoryBase';

our $VERSION = "0.001";

Game::Common->load_classes('Game::Repository', 'Repository/*.pm');

has '+char_cache' => (
	default => sub { Game::Repository::CharCache->new },
);

has '+ability_data' => (
	default => sub { Game::Repository::AbilityData->new },
);

has '+class_data' => (
	default => sub { Game::Repository::ClassData->new },
);

has '+lore_data' => (
	default => sub { Game::Repository::LoreData->new },
);

has '+schema' => (
	default => sub { Game::Repository::Schema->new },
);

has '+actor_unit' => (
	default => sub { Game::Repository::ActorUnit->new },
);

has '+battle_unit' => (
	default => sub { Game::Repository::BattleUnit->new },
);

sub bootstrap ($class)
{
	add_to_container('repo', $class->new);
}

1;
