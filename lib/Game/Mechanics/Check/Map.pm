package Game::Mechanics::Check::Map;

use Game::Mechanics::Check;
use Exporter qw(import);

use header;

our @EXPORT_OK = qw(
	can_move_to
	can_see
);

sub can_move_to ($map, $position1, $position2)
{
	return Game::Mechanics::Check->check(
		'err.cannot_move',
		all { $map->check_can_be_accessed($_->@*) } $position1, $position2
	);
}

# get tile sides in range as an array, like this:
# [ ]|[ ]|[ ]|[ ]
# (sides marked with |)
sub _get_tile_sides ($from, $to)
{
	return $to >= $from
		? ($from + 1 .. $to)
		: ($to + 1 .. $from)
		;
}

sub _can_see_inner ($map, $position1, $position2)
{
	my $coeff_x = ($position2->[1] - $position1->[1]) / ($position2->[0] - $position1->[0]);

	my $partial_y = $position1->[1] - $position1->[0] * $coeff_x;
	foreach my $pos_x (_get_tile_sides($position1->[0], $position2->[0])) {
		my $pos_y = $partial_y + $pos_x * $coeff_x;

		return false unless $map->check_within_map($pos_x, $pos_y)
			&& $map->check_within_map($pos_x - 1, $pos_y);
	}

	my $partial_x = $position1->[0] - $position1->[1] / $coeff_x;
	foreach my $pos_y (_get_tile_sides($position1->[1], $position2->[1])) {
		my $pos_x = $partial_x + $pos_y / $coeff_x;

		return false unless $map->check_within_map($pos_x, $pos_y)
			&& $map->check_within_map($pos_x, $pos_y - 1);
	}

	return true;
}

sub can_see
{
	return Game::Mechanics::Check->check(
		'err.not_in_los',
		_can_see_inner(@_),
	);
}

