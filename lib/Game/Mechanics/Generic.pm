package Game::Mechanics::Generic;

use header;

sub calculate_distance ($self, $start_x, $start_y, $end_x, $end_y)
{
	return sqrt(($start_x - $end_x) ** 2 + ($start_y - $end_y) ** 2);
}

