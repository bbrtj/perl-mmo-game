package Game::Model::Battle;

use header;
use Moose;
use Game::Types qw(Maybe LoreId PositiveInt);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

my %sizes = qw(
	small 80
	medium 100
	large 120
);

my %types = qw(
	wide 0.8
	square 1
	tall 1.2
);

has 'location_id' => (
	is => 'ro',
	isa => Maybe [LoreId],
	required => 1,
);

has 'size_x' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'size_y' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'turn' => (
	is => 'ro',
	isa => PositiveInt,
	default => sub { 0 },
);

sub set_size ($self, $size, $type)
{
	$self->set_size_x($sizes{$size});
	$self->set_size_y($sizes{$size} * $types{$type});
	return;
}

__PACKAGE__->_register;
