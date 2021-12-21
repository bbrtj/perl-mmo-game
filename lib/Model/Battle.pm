package Model::Battle;

use Moose;
use Types;

use header;

with 'Model', 'Model::Role::Stored';

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
	isa => Types::Maybe [Types::LoreId],
	required => 1,
);

has 'size_x' => (
	is => 'ro',
	isa => Types::PositiveInt,
	required => 1,
);

has 'size_y' => (
	is => 'ro',
	isa => Types::PositiveInt,
	required => 1,
);

has 'turn' => (
	is => 'ro',
	isa => Types::PositiveInt,
	default => sub { 0 },
);

sub set_size ($self, $size, $type)
{
	$self->set_size_x($sizes{$size});
	$self->set_size_y($sizes{$size} * $types{$type});
	return;
}

__PACKAGE__->_register;
