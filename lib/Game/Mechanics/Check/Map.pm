package Game::Mechanics::Check::Map;

use Game::Mechanics::Check;
use POSIX qw(ceil);

use header;

sub is_within_map ($self, $location, @positions)
{
	my $map = $location->map;
	my $coords = $map->coordinates;
	my ($mx, $my) = ($map->size_x, $map->size_y);

	my $result = 1;
	for my $position (@positions) {
		my ($x, $y) = @$position;

		$result &&=
			(0 <= $x < $mx && 0 <= $y < $my)
			&& $coords->[int $y][int $x];

		last unless $result;
	}

	# map goes from 0 to size_*
	return Game::Mechanics::Check->check(
		'err.invalid_coordinate',
		$result
	);
}

sub can_move_to ($self, $location, $position1, $position2)
{
	return Game::Mechanics::Check->gather(
		'err.cannot_move',
		$self->is_within_map($location, $position1, $position2),
	);
}

sub can_see ($self, $location, $position1, $position2)
{
	# my @pos;
	my $check = sub {
		my $check_xy = sub ($pos_x, $pos_y) {
			state $coordinates = $location->map->coordinates;
			# push @pos, [$pos_x, $pos_y];
			return $coordinates->[$pos_y][$pos_x];
		};

		my $check_neighbours = sub ($which, @points) {
			my $checked = $check_xy->(@points);
			return $checked if $points[$which] == 0;

			$points[$which] -= 1;
			return $checked && $check_xy->(@points);
		};

		my @central = $position1->@*;

		my $check_for_x = sub ($pos_x) {
			state $coeff = (($position2->[1] - $central[1]) / ($position2->[0] - $central[0]));

			my $pos_y = int ($central[1] + ($pos_x - $central[0]) * $coeff);
			return $check_neighbours->(0 => $pos_x, $pos_y);
		};

		my $check_for_y = sub ($pos_y) {
			state $coeff = (($position2->[0] - $central[0]) / ($position2->[1] - $central[1]));

			my $pos_x = int ($central[0] + ($pos_y - $central[1]) * $coeff);
			return $check_neighbours->(1 => $pos_x, $pos_y);
		};

		my $loop_coords = sub ($from, $to, $run_func) {
			my @loop_list = $to >= $from
				? ceil($from) .. int($to)
				: ceil($to) .. int($from);

			for my $current (@loop_list) {
				return 0 if !$run_func->($current);
			}

			return 1;
		};

		return 0 unless $loop_coords->($position1->[0], $position2->[0], $check_for_x);
		return 0 unless $loop_coords->($position1->[1], $position2->[1], $check_for_y);
		return 1;
	};

	# $check->();
	# warn $location->map->to_string_and_mark(@pos) if @pos;
	return Game::Mechanics::Check->gather(
		'err.not_in_los',
		$self->is_within_map($location, $position1, $position2),
		$check
	);
}

