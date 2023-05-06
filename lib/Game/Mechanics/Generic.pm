package Game::Mechanics::Generic;

use Math::Trig qw(pi atan);

use header;

# use Inline 'C';

sub calculate_distance ($self, $start_x, $start_y, $end_x, $end_y)
{
	return sqrt(($start_x - $end_x)**2 + ($start_y - $end_y)**2);
}

sub calculate_diagonal ($self, $x_dist, $y_dist)
{
	return sqrt($x_dist * $x_dist + $y_dist * $y_dist);
}

# (in radians)
sub calculate_angle ($self, $x_dist, $y_dist)
{
	# https://math.stackexchange.com/questions/1183357/when-do-you-add-180-to-the-directional-angle/3003263#3003263
	return pi if $y_dist == 0 && $x_dist <= 0;
	return 2 * atan($y_dist / ($x_dist + sqrt($x_dist * $x_dist + $y_dist * $y_dist)));
}

sub calculate_angle_and_diagonal ($self, $start_x, $start_y, $end_x, $end_y)
{
	my $x_dist = $end_x - $start_x;
	my $y_dist = $end_y - $start_y;
	my $diagonal = sqrt($x_dist * $x_dist + $y_dist * $y_dist);
	my $angle = $y_dist == 0 && $x_dist <= 0 ? pi : 2 * atan($y_dist / ($x_dist + $diagonal));

	return ($angle, $diagonal);
}

sub find_frontal_point ($self, $x, $y, $angle, $distance)
{
	return ($x + $distance * cos $angle, $y + $distance * sin $angle);
}

__DATA__

__C__

double calculate_distance(SV *self, double x1, double y1, double x2, double y2)
{
	return pow(pow(x2 - x1, 2) + pow(y2 - y1, 2), 0.5);
}

double calculate_diagonal(SV *self, double x_dist, double y_dist)
{
	return pow(x_dist * x_dist + y_dist * y_dist, 0.5);
}

void calculate_angle_and_diagonal (SV *self, double start_x, double start_y, double end_x, double end_y)
{
	double x_dist = end_x - start_x;
	double y_dist = end_y - start_y;
	double diagonal = pow(x_dist * x_dist + y_dist * y_dist, 0.5);
	double angle;
	if (y_dist == 0 && y_dist <= 0) angle = M_PI;
	else angle = 2 * atan(y_dist / (x_dist + diagonal));

	Inline_Stack_Vars;

	Inline_Stack_Reset;
	Inline_Stack_Push(sv_2mortal(newSVnv(angle)));
	Inline_Stack_Push(sv_2mortal(newSVnv(diagonal)));
	Inline_Stack_Done;
}

void find_frontal_point (SV *self, double x, double y, double angle, double distance)
{
	Inline_Stack_Vars;

	Inline_Stack_Reset;
	Inline_Stack_Push(sv_2mortal(newSVnv(x + distance * cos(angle))));
	Inline_Stack_Push(sv_2mortal(newSVnv(y + distance * sin(angle))));
	Inline_Stack_Done;
}

