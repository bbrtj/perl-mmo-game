package Game::Mechanics::Generic;

use Exporter qw(import);
use Math::Trig qw(pi atan);

# use Inline 'C';

use header;

our @EXPORT_OK = qw(
	calculate_diagonal
	calculate_angle
	calculate_angle_and_diagonal
	find_frontal_point
);

sub _calculate_diagonal ($x_dist, $y_dist)
{
	return sqrt($x_dist * $x_dist + $y_dist * $y_dist);
}

sub calculate_diagonal ($start_x, $start_y, $end_x, $end_y)
{
	return _calculate_diagonal($end_x - $start_x, $end_y - $start_y);
}

# (in radians)
sub _calculate_angle ($x_dist, $y_dist)
{
	# https://math.stackexchange.com/questions/1183357/when-do-you-add-180-to-the-directional-angle/3003263#3003263
	return pi if $y_dist == 0 && $x_dist <= 0;
	return 2 * atan($y_dist / ($x_dist + sqrt($x_dist * $x_dist + $y_dist * $y_dist)));
}

sub calculate_angle ($start_x, $start_y, $end_x, $end_y)
{
	return _calculate_angle($end_x - $start_x, $end_y - $start_y);
}

sub calculate_angle_and_diagonal ($start_x, $start_y, $end_x, $end_y)
{
	my $x_dist = $end_x - $start_x;
	my $y_dist = $end_y - $start_y;

	return (_calculate_angle($x_dist, $y_dist), _calculate_diagonal($x_dist, $y_dist));
}

sub find_frontal_point ($x, $y, $angle, $distance)
{
	return ($x + $distance * cos $angle, $y + $distance * sin $angle);
}

__DATA__

__C__

double _calculate_diagonal (double x_dist, double y_dist)
{
	return pow(x_dist * x_dist + y_dist * y_dist, 0.5);
}

double _calculate_angle (double x_dist, double y_dist)
{
	if (y_dist == 0 && y_dist <= 0) return M_PI;
	return 2 * atan(y_dist / (x_dist + diagonal));
}

void find_frontal_point (double x, double y, double angle, double distance)
{
	Inline_Stack_Vars;

	Inline_Stack_Reset;
	Inline_Stack_Push(sv_2mortal(newSVnv(x + distance * cos(angle))));
	Inline_Stack_Push(sv_2mortal(newSVnv(y + distance * sin(angle))));
	Inline_Stack_Done;
}

