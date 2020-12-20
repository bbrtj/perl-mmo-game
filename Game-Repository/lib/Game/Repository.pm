package Game::Repository;

use header;
use Moo;
use Game::Common::Container qw(add_to_container);
use Game::Repository::CharCache;
use Game::Repository::AbilityData;
use Game::Repository::Schema;

no header;

extends 'Game::RepositoryBase';

our $VERSION = "0.001";

has '+char_cache' => (
	default => sub { Game::Repository::CharCache->new },
);

has '+ability_data' => (
	default => sub { Game::Repository::AbilityData->new },
);

has '+schema' => (
	default => sub { Game::Repository::Schema->new },
);

sub bootstrap ($class)
{
	add_to_container('repo', $class->new);
}

1;
