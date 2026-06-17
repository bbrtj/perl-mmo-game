package Game::Mechanics::Distance;

use Exporter qw(import);

# use Inline 'C';

use header;

our @EXPORT_OK = qw(
	calculate_distance
	is_in_range
	find_actors_in_range
);

sub calculate_distance ($start_x, $start_y, $end_x, $end_y)
{
	return sqrt(($start_x - $end_x)**2 + ($start_y - $end_y)**2);
}

sub is_in_range ($pos1, $pos2, $range)
{
	return calculate_distance(@$pos1, @$pos2) <= $range;
}

sub find_actors_in_range ($server, $x, $y, $range)
{
	my $location = $server->location;

	return grep {
		defined
	} map {
		$location->get_actor($_)
	} $server->find_in_radius($x, $y, $range)->@*;
}

__DATA__

__C__

double calculate_distance(double x1, double y1, double x2, double y2)
{
	return pow(pow(x2 - x1, 2) + pow(y2 - y1, 2), 0.5);
}

