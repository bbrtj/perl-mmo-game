package Game::Model::Player;

use header;
use Moose;
use Game::Types qw(Uuid LoreId NonEmptySimpleStr Bool DateTime Maybe);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'user_id' => (
	is => 'ro',
	isa => Uuid,
	required => 1,
);

has 'class_id' => (
	is => 'ro',
	isa => LoreId,
	required => 1,
);

has 'name' => (
	is => 'ro',
	isa => NonEmptySimpleStr->where(q{ length $_ <= 32 }),
	required => 1,
);

has 'online' => (
	is => 'ro',
	isa => Bool,
	default => sub { 0 },
);

has 'created_at' => (
	is => 'ro',
	isa => DateTime,
	coerce => 1,
	default => sub { time },
);

has 'last_online' => (
	is => 'ro',
	isa => Maybe [DateTime],
	coerce => 1,
	default => sub { undef },
);

__PACKAGE__->_register;
