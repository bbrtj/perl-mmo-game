package Game::Mechanics::Movement;

use header;

# $movement is Game::Object::Movement
sub move ($self, $movement, $elapsed)
{
	my $variables = $movement->actor->variables;
	my $distance = ($elapsed - $movement->time) * $movement->speed;

	my $new_x = $variables->pos_x + $movement->x_coeff * $distance;
	my $new_y = $variables->pos_y + $movement->y_coeff * $distance;
	if ($movement->map->check_can_be_accessed($new_x, $new_y)) {
		$variables->set_pos_x($new_x);
		$variables->set_pos_y($new_y);
		$movement->set_time($elapsed);

		return !!1;
	}

	return !!0;
}

