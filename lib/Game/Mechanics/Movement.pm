package Game::Mechanics::Movement;

use header;
use Game::Config;

# $movement is Game::Object::Movement
sub move ($self, $movement, $elapsed, $map)
{
	my $variables = $movement->variables;
	$elapsed = $movement->eta
		if $movement->eta < $elapsed;

	my $distance = ($elapsed - $movement->time) * $movement->speed;

	my $coeffs = $movement->coeffs;
	my $new_x = $variables->pos_x + $coeffs->[0] * $distance;
	my $new_y = $variables->pos_y + $coeffs->[1] * $distance;

	if (!$map->check_can_be_accessed($new_x, $new_y)) {
		# try to find position closer to the wall / ledge. It's okay to be slow and precise here
		# TODO: deep recursion??
		# $self->move($movement, $elapsed - Game::Config->config->{map_precision} / $movement->speed, $map);
		return !!0;
	}

	$variables->set_pos_x($new_x);
	$variables->set_pos_y($new_y);
	$movement->set_time($elapsed);

	return $distance > 0;
}

