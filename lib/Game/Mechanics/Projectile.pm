package Game::Mechanics::Projectile;

use Game::Config;
use Game::Mechanics::Generic;
use List::Util qw(min);

use header;

# $projectile is Game::Object::Projectile
# returns whether a projectile kept going without hitting a wall - can check
# whether the projectile run out of range with ->finished method
sub travel ($self, $projectile, $map, $elapsed = server_time)
{
	my ($new_x, $new_y) = Game::Mechanics::Generic->find_frontal_point(
		$projectile->xy,
		$projectile->angle,
		($elapsed - $projectile->time) * $projectile->speed,
	);

	# NOTE: no rounding of position - if projectiles hit something, apply their
	# effect there
	return false unless $map->check_can_be_accessed($new_x, $new_y);

	$projectile->set_time($elapsed);
	$projectile->set_x($new_x);
	$projectile->set_y($new_y);

	return true;
}

