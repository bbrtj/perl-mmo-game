package Game::Mechanics::Distance;

use Game::Mechanics::Generic;

use header;

sub is_in_range ($self, $pos1, $pos2, $range)
{
	return Game::Mechanics::Generic->calculate_distance(@$pos1, @$pos2) <= $range;
}

sub find_actors_in_range ($self, $server, $x, $y, $range)
{
	my $location = $server->location;

	return grep {
		defined
	} map {
		$location->get_actor($_)
	} $server->find_in_radius($x, $y, $range)->@*;
}

