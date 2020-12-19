package Game::RepositoryBase;

use header;
use Moo;

no header;

has 'cache' => (
	is => 'ro',
);

has 'data' => (
	is => 'ro',
);

has 'schema' => (
	is => 'ro',
);

1;
