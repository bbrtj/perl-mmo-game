package Game::Mechanics::Check::Map;

use Game::Mechanics::Check;

use header;

sub can_move_to ($self, $map, $position1, $position2)
{
	return Game::Mechanics::Check->check(
		'err.cannot_move',
		all { $map->check_can_be_accessed(@$_) } $position1, $position2
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

	my $checks_for_x = sub ($pos_x) {
		state $partial = $position1->[1] - $position1->[0] * $coeff_x;
		my $pos_y = $partial + $pos_x * $coeff_x;
		return ([$pos_x, $pos_y], [$pos_x - 1, $pos_y]);
	};

	my $checks_for_y = sub ($pos_y) {
		state $partial = $position1->[0] - $position1->[1] / $coeff_x;
		my $pos_x = $partial + $pos_y / $coeff_x;
		return ([$pos_x, $pos_y], [$pos_x, $pos_y - 1]);
	};

	# NOTE OPTIMIZATION: @coords can be calculated in C
	my @coords = (
		(map { $checks_for_x->($_) } $self->_get_tile_sides($position1->[0], $position2->[0])),
		(map { $checks_for_y->($_) } $self->_get_tile_sides($position1->[1], $position2->[1]))
	);

	return Game::Mechanics::Check->check(
		'err.not_in_los',
		all { $map->check_within_map($_->@*) } @coords
	);
}

