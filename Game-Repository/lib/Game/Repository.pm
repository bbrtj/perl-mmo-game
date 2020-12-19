package Game::Repository;

use header;
use Moo;
use Game::Common::Container qw(add_to_container);
use Game::Repository::Cache;
use Game::Repository::Data;
use Game::Repository::Schema;

no header;

extends 'Game::RepositoryBase';

our $VERSION = "0.001";

has '+cache' => (
	default => sub { Game::Repository::Cache->new },
);

has '+data' => (
	default => sub { Game::Repository::Data->new },
);

has '+schema' => (
	default => sub { Game::Repository::Schema->new },
);

sub bootstrap ($class)
{
	add_to_container('repo', $class->new);
}

1;
