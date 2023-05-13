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
	my $rounded = !!0;

	my ($distance, $new_x, $new_y);
	while ('inaccessible') {
		$distance = ($elapsed - $movement->time) * $movement->speed;
		($new_x, $new_y) = Game::Mechanics::Generic->find_frontal_point(
			$variables->pos_x,
			$variables->pos_y,
			$movement->angle,
			$distance
		);

		last if $map->check_can_be_accessed($new_x, $new_y);

		# try to find position closer to the wall / ledge. It's okay to be slower but precise here
		$elapsed -= Game::Config->config->{map_precision} * $movement->speed;
		$rounded = !!1;
	}

	$variables->set_pos_x($new_x);
	$variables->set_pos_y($new_y);
	$movement->set_time($elapsed);

	return $distance > 0 && !$rounded;
}

