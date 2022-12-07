package Game::Mechanics::Generic;

use header;

# use Inline 'C';

sub calculate_distance ($self, $start_x, $start_y, $end_x, $end_y)
{
	return sqrt(($start_x - $end_x)**2 + ($start_y - $end_y)**2);
}

__DATA__

__C__

double calculate_distance(SV *self, double x1, double y1, double x2, double y2)
{
	return pow(pow(x2 - x1, 2) + pow(y2 - y1, 2), 0.5);
}

