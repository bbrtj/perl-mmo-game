package Game::Model::Role::Stored;

use header;
use Moo::Role;
use Game::Types qw(Uuid);

no header;

has 'id' => (
	is => 'ro',
	isa => Uuid,
	coerce => 1,
	default => sub { undef },
);

1;
