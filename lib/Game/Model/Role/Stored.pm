package Game::Model::Role::Stored;

use Moo::Role;
use Types;

use header;

has 'id' => (
	is => 'ro',
	isa => Types::Ulid,
	coerce => 1,
	default => sub { undef },
);

# dirty columns - for updating the model in db
has '_dirty' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
);
