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

sub can_see ($self, $location, $position1, $position2)
{
	my $check = $location->map->inline_check_within_map;
	my @central = $position1->@*;
	my $coeff_x = ($position2->[1] - $central[1]) / ($position2->[0] - $central[0]);

	my $check_for_x = sub ($pos_x) {
		my $pos_y = int ($central[1] + ($pos_x - $central[0]) * $coeff_x);
		return $check->($pos_x, $pos_y) && ($pos_x == 0 || $check->($pos_x - 1, $pos_y));
	};

	my $check_for_y = sub ($pos_y) {
		my $pos_x = int ($central[0] + ($pos_y - $central[1]) / $coeff_x);
		return $check->($pos_x, $pos_y) && ($pos_y == 0 || $check->($pos_x, $pos_y - 1));
	};

	my $loop_coords = sub ($from, $to, $run_func) {
		# $from and $to will be tested as a part of checking the lower coordinate ($pos - 1 above)
		my @loop_list = $to >= $from
			? $from + 1 .. $to
			: $to + 1 .. $from;

		for my $current (@loop_list) {
			return 0 if !$run_func->($current);
		}

		return 1;
	};

	return Game::Mechanics::Check->gather(
		'err.not_in_los',
		$loop_coords->($position1->[0], $position2->[0], $check_for_x),
		$loop_coords->($position1->[1], $position2->[1], $check_for_y),
	);

}

