package Game::Model::Player;

use header;
use Moose;
use Game::Types qw(Uuid Bool DateTime Maybe);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'user_id' => (
	is => 'ro',
	isa => Uuid,
	required => 1,
);

has 'online' => (
	is => 'ro',
	isa => Bool,
	default => sub { 0 },
);

has 'last_online' => (
	is => 'ro',
	isa => Maybe [DateTime],
	coerce => 1,
	default => sub { undef },
);

has 'created_at' => (
	is => 'ro',
	isa => DateTime,
	coerce => 1,
	default => sub { time },
);

__PACKAGE__->_register;
