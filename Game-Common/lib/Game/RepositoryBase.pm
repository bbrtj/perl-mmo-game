package Game::RepositoryBase;

use header;
use Moo;
use Game::Types qw(ConsumerOf);

no header;

has 'char_cache' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Repository::Role::Resource'],
);

has 'ability_data' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Repository::Role::Resource'],
);

has 'schema' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Repository::Role::Resource'],
);

1;
