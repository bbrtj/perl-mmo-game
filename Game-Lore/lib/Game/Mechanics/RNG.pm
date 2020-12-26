package Game::Mechanics::RNG;

use header;
use Game::RNG;

no header;

sub is_within_map ($self, $battle, @position)
{
	my ($x, $y) = @position;
	my ($mx, $my) = ($battle->battle->size_x, $battle->battle->size_y);

	# map goes from 0 to size_*
	return $x >= 0 && $x < $mx && $y >= 0 && $y < $my;
}

sub can_move_to ($self, $battle, $id, @position)
{
}

1;

