package Game::Mechanics::Battle::Range;

use header;
use Game::Types qw(InstanceOf);

use BATTLE_TYPE => InstanceOf ['Game::Unit::Battle'];

no header;

sub is_within_map ($self, $battle, @position)
{
	BATTLE_TYPE->assert_valid($battle);
	my ($x, $y) = @position;
	my ($mx, $my) = ($battle->battle->size_x, $battle->battle->size_y);

	# map goes from 0 to size_*
	return $x >= 0 && $x < $mx && $y >= 0 && $y < $my;
}

sub can_move_to ($self, $battle, $id, @position)
{
	return is_within_map($battle);
}

1;