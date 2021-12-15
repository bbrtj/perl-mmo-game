package Game::Mechanics::Check::Map;

use Game::Mechanics::Check::Carry;

use header;

sub is_within_map ($self, $battle, $position)
{
	my ($x, $y) = @$position;
	my ($mx, $my) = ($battle->battle->size_x, $battle->battle->size_y);

	# map goes from 0 to size_*
	return Game::Mechanics::Check::Carry->gather(
		'invalid_coordinate',
		0 <= $x < $mx && 0 <= $y < $my
	);
}

sub can_move_to ($self, $battle, $id, $position)
{
	return Game::Mechanics::Check::Carry->gather(
		'cannot_move',
		$self->is_within_map($battle, $position)
	);
}

sub can_see ($self, $battle, $id, $position)
{
	return Game::Mechanics::Check::Carry->gather(
		'not_in_los',
		$self->is_within_map($battle, $position)
	);
}
