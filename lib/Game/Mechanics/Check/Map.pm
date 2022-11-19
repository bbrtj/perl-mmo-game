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

sub _loop_coords ($self, $from, $to, $run_func)
{
	# $from and $to will be tested as a part of checking the lower coordinate ($pos - 1 above)
	my @loop_list = $to >= $from
		? $from + 1 .. $to
		: $to + 1 .. $from;

	foreach (@loop_list) {
		return 0 if !$run_func->($_);
	}

	return 1;
}

sub can_see ($self, $location, $position1, $position2)
{
	my $map = $location->map;
	my @central = $position1->@*;
	my $coeff_x = ($position2->[1] - $central[1]) / ($position2->[0] - $central[0]);

	my $check_for_x = sub ($pos_x) {
		state $partial = $central[1] - $central[0] * $coeff_x;
		my $pos_y = $partial + $pos_x * $coeff_x;
		return $map->check_within_map($pos_x, $pos_y) && ($pos_x == 0 || $map->check_within_map($pos_x - 1, $pos_y));
	};

	my $check_for_y = sub ($pos_y) {
		state $partial = $central[0] - $central[1] / $coeff_x;
		my $pos_x = $partial + $pos_y / $coeff_x;
		return $map->check_within_map($pos_x, $pos_y) && ($pos_y == 0 || $map->check_within_map($pos_x, $pos_y - 1));
	};

	return Game::Mechanics::Check->check(
		'err.not_in_los',
		$self->_loop_coords($position1->[0], $position2->[0], $check_for_x) &&
		$self->_loop_coords($position1->[1], $position2->[1], $check_for_y)
	);

}

