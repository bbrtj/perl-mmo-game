package Game::Mechanics::Movement;

use Exporter qw(import);
use Game::Config;
use Game::Mechanics::Generic qw(find_frontal_point);
use List::Util qw(min);

use header;

our @EXPORT_OK = qw(
	move
);

# $movement is Game::Object::Movement
sub move ($movement, $map, $elapsed = server_time)
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

