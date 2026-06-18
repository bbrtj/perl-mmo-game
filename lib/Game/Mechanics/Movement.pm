package Game::Mechanics::Movement;

use Exporter qw(import);
use Game::Config;
use Game::Mechanics::Generic qw(find_frontal_point);
use List::Util qw(min);

use header;

our @EXPORT_OK = qw(
	move_actor
	move_projectile
);

# $movement is Game::Object::Movement
# returns whether the actor kept going
sub move_actor ($movement, $map, $elapsed = server_time)
{
	my $variables = $movement->variables;
	$elapsed = min($elapsed, $movement->eta);

	my $distance = ($elapsed - $movement->get_time) * $movement->speed;
	my ($new_x, $new_y) = find_frontal_point(
		$variables->xy,
		$movement->angle,
		$distance
	);

	return false
		unless $map->check_can_be_accessed($new_x, $new_y);

	$variables->set_pos_x($new_x);
	$variables->set_pos_y($new_y);
	$movement->set_time($elapsed);

	return $distance > 0;
}

# $projectile is Game::Object::Projectile
# returns whether a projectile kept going without hitting a wall - can check
# whether the projectile run out of range with ->finished method
sub move_projectile ($projectile, $map, $elapsed = server_time)
{
	my ($new_x, $new_y) = find_frontal_point(
		$projectile->xy,
		$projectile->angle,
		($elapsed - $projectile->get_time) * $projectile->speed,
	);

	return false
		unless $map->check_within_map($new_x, $new_y);

	$projectile->set_x($new_x);
	$projectile->set_y($new_y);
	$projectile->set_time($elapsed);

	return true;
}

