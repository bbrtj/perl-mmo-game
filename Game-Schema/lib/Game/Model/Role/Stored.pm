package Game::Model::Role::Stored;

use header;
use Moo::Role;
use Game::Types qw(Uuid);

no header;

has 'uuid' => (
	is => 'ro',
	isa => Uuid,
	coerce => 1,
	default => sub { undef },
);

1;
