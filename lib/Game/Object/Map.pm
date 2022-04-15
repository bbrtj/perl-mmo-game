package Game::Object::Map;

use My::Moose;
use Types;
use Form::Tiny::Utils qw(trim);
use Storable qw(dclone);

use header;

use constant NOT_MAP => 0;
use constant MAP => 1;
use constant INACCESSIBLE => 2;

use constant CHARACTERS => {
	'.' => NOT_MAP,
	'O' => MAP,
	'X' => INACCESSIBLE,
};

has 'coordinates' => (
	is => 'ro',
	isa => Types::ArrayRef[Types::ArrayRef[Types::Int]],
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

sub from_string ($self, $map_str)
{
	my @map_lines = grep { /\S/ } map { trim $_ } split "\n", $map_str;
	my @map_size = (length $map_lines[0], scalar @map_lines);
	my @map;

	for my $line (@map_lines) {
		my @bools = map { CHARACTERS->{$_} // die "Invalid map character $_" } split '', $line;

		die "invalid map size on line ($line)"
			if @bools != $map_size[0];

		push @map, \@bools;
	}

	return $self->new(
		coordinates => \@map,
		size_x => $map_size[0],
		size_y => $map_size[1],
	);
}

sub inline_check_within_map ($self)
{
	my ($sx, $sy) = ($self->size_x, $self->size_y);
	my @coords = $self->coordinates->@*;

	return sub ($x, $y) {
		return 0 <= $x < $sx
			&& 0 <= $y < $sy
			&& $coords[$y][$x] != NOT_MAP;
	};
}

sub check_within_map ($self, $x, $y)
{
	my $sub = $self->inline_check_within_map;
	return $sub->($x, $y);
}

sub check_can_be_accessed ($self, $x, $y)
{
	return 0 <= $x < $self->size_x
		&& 0 <= $y < $self->size_y
		&& $self->coordinates->[$y][$x] == MAP;
}

sub to_string_and_mark ($self, @positions)
{
	my @lines;
	my %characters_rev = map { CHARACTERS->{$_} => $_ } keys CHARACTERS->%*;
	my $coordinates = dclone $self->coordinates;

	for my $pos (@positions) {
		$coordinates->[$pos->[1]][$pos->[0]] = 'mark';
	}

	for my $coords ($coordinates->@*) {
		push @lines, join '', map { $characters_rev{$_} // '@' } $coords->@*;
	}

	return join "\n", @lines;
}

