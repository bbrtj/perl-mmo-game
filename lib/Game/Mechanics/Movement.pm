package Game::Mechanics::Movement;

use header;
use Game::Config;

# $movement is Game::Object::Movement
sub move ($self, $movement, $elapsed, $map)
{
	my $variables = $movement->variables;
	my $coeffs = $movement->coeffs;
	$elapsed = $movement->eta
		if $movement->eta < $elapsed;

	# was this result rounded?
	my $rounded = !!0;

	my ($distance, $new_x, $new_y);
	while (-inaccessible) {
		$distance = ($elapsed - $movement->time) * $movement->speed;
		$new_x = $variables->pos_x + $coeffs->[0] * $distance;
		$new_y = $variables->pos_y + $coeffs->[1] * $distance;

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

