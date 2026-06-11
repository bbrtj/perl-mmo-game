package Game::Mechanics::Movement;

use Game::Config;
use Game::Mechanics::Generic;
use List::Util qw(min);

use header;

# $movement is Game::Object::Movement
sub move ($self, $movement, $map, $elapsed = server_time)
{
	my $variables = $movement->variables;
	$elapsed = min($elapsed, $movement->eta);

	# was this result rounded?
	my $rounded = false;

	my ($distance, $new_x, $new_y);
	my $time = $movement->time;
	while ($elapsed >= $time) {
		$distance = ($elapsed - $time) * $movement->speed;
		($new_x, $new_y) = Game::Mechanics::Generic->find_frontal_point(
			$variables->xy,
			$movement->angle,
			$distance
		);

		last if $map->check_can_be_accessed($new_x, $new_y);

		# try to find position closer to the wall / ledge. It's okay to be slower but precise here
		$elapsed -= Game::Config->map_precision * $movement->speed;
		$rounded = true;
	}

	$variables->set_pos_x($new_x);
	$variables->set_pos_y($new_y);
	$movement->set_time($elapsed);

	return $distance > 0 && !$rounded;
}

