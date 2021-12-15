package Game::Mechanics::Check::Ability;

use Game::Mechanics::Check::Map;
use Game::Mechanics::Distance;
use Game::Mechanics::Check::Carry;
use Game::Mechanics::Battle::Target;

use header;

sub in_range ($self, $battle, $actor, $ability, $target)
{
	my $range = $ability->range // 5;    # TODO weapon range
	my $position = Game::Mechanics::Battle::Target->get_position($battle, $target);
	my $pos_cur = [$actor->contestant->pos_x, $actor->contestant->pos_y];

	return Game::Mechanics::Check::Carry->gather(
		'out_of_range',
		Game::Mechanics::Distance->is_in_range($pos_cur, $position, $range),
		Game::Mechanics::Check::Map->can_see($battle, $position)
	);
}

sub valid_target ($self, $battle, $actor, $ability, $target)
{
	return Game::Mechanics::Check::Carry->gather(
		'invalid_target',
		Game::Mechanics::Battle::Target->valid_target($self, $battle, $actor, $ability, $target)
	);
}

