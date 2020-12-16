package Game::Model::Player;

use Mojo::Base -signatures;
use Moose;
use Game::Types qw(Uuid LoreId NonEmptySimpleStr Bool DateTime Maybe);

with 'Game::Model';

has 'user_id' => (
	is => 'rw',
	isa => Uuid,
	required => 1,
);

has 'class_id' => (
	is => 'rw',
	isa => LoreId,
	required => 1,
);

has 'name' => (
	is => 'rw',
	isa => NonEmptySimpleStr->where(q{ length $_ <= 32 }),
	required => 1,
);

has 'online' => (
	is => 'rw',
	isa => Bool,
	default => sub { 0 },
);

has 'created_at' => (
	is => 'ro',
	isa => DateTime,
	default => sub { time },
);

has 'last_online' => (
	is => 'rw',
	isa => Maybe[DateTime],
	default => sub { undef },
);

__PACKAGE__->_register;
