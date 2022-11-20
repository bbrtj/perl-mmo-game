package Game::Mechanics::Check::Map;

use Game::Mechanics::Check;

use header;

sub can_move_to ($self, $location, $position1, $position2)
{
	return Game::Mechanics::Check->gather(
		'err.cannot_move',
		$location->map->check_can_be_accessed(@$position1),
		$location->map->check_can_be_accessed(@$position2),
	);
}

# get tile sides in range as an array, like this:
# [ ]|[ ]|[ ]|[ ]
# (sides marked with |)
sub _get_tile_sides ($self, $from, $to)
{
	return $to >= $from
		? ($from + 1 .. $to)
		: ($to + 1 .. $from)
		;
}

sub can_see ($self, $location, $position1, $position2)
{
	my $map = $location->map;
	my $coeff_x = ($position2->[1] - $position1->[1]) / ($position2->[0] - $position1->[0]);
	my $partial_pos_y = $position1->[1] - $position1->[0] * $coeff_x;

	my @coords = ($position1, $position2);
	for my $pos_x ($self->_get_tile_sides($position1->[0], $position2->[0])) {
		my $pos_y = $partial_pos_y + $pos_x * $coeff_x;
		push @coords,
			[$pos_x, $pos_y],
			[$pos_x - 1, $pos_y],
			;
	}

	# NOTE: optimization: @coords can be calculated in C

	return Game::Mechanics::Check->check(
		'err.not_in_los',
		all { $map->check_within_map($_->@*) } @coords
	);
}

