package Game::Model::Character;

use header;
use Moose;
use Game::Types qw(Maybe Uuid LoreId NonEmptySimpleStr);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'player_id' => (
	is => 'ro',
	isa => Maybe [Uuid],
	default => sub { undef },
);

has 'npc_id' => (
	is => 'ro',
	isa => Maybe [Uuid],
	default => sub { undef },
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

has 'stats' => (
	is => 'ro',
	isa => NonEmptySimpleStr,
	required => 1,
);

sub is_player ($self)
{
	return defined $self->player_id;
}

__PACKAGE__->_register;
