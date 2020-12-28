package Game::Mechanics::Distance;

use header;

sub is_in_range ($self, $pos1, $pos2, $range)
{
	my ($sx, $sy, $ex, $ey) = (@$pos1, @$pos2);
	return sqrt(($sx - $ex)**2 + ($sy - $ey)**2) <= $range;
}

1;

