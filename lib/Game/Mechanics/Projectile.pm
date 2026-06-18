package Game::Mechanics::Projectile;

use Exporter qw(import);
use Game::Config;
use Game::Mechanics::Generic qw(find_frontal_point);
use List::Util qw(min);

use header;

our @EXPORT_OK = qw(
	travel
);

# $projectile is Game::Object::Projectile
# returns whether a projectile kept going without hitting a wall - can check
# whether the projectile run out of range with ->finished method
sub travel ($projectile, $map, $elapsed = server_time)
{
	my ($new_x, $new_y) = find_frontal_point(
		$projectile->xy,
		$projectile->angle,
		($elapsed - $projectile->get_time) * $projectile->speed,
	);

	# NOTE: no rounding of position - if projectiles hit something, apply their
	# effect there
	return false unless $map->check_within_map($new_x, $new_y);

	$projectile->set_time($elapsed);
	$projectile->set_x($new_x);
	$projectile->set_y($new_y);

	return true;
}

