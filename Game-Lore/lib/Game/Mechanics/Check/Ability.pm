package Game::Mechanics::Check::Ability;

use header;
use Game::Types qw(InstanceOf);
use Game::Mechanics::Check::Map;
use Game::Mechanics::Distance;
use Game::Mechanics::Check::Carry;
use Game::Mechanics::Battle::Target;

no header;

sub in_range ($self, $battle, $character, $ability, $target)
{
	my $range = $ability->range // 5;    # TODO weapon range
	my $contestant = $battle->find_contestant($character->id);
	my $position = Game::Mechanics::Battle::Target->get_position($battle, $target);
	my $pos_cur = [$contestant->[0]->pos_x, $contestant->[0]->pos_y];

	return Game::Mechanics::Check::Carry->gather(
		'out_of_range',
		Game::Mechanics::Distance->is_in_range($pos_cur, $position, $range),
		Game::Mechanics::Check::Map->can_see($battle, $position)
	);
}

sub valid_target ($self, $battle, $ability, $target)
{
	my $at_character = Game::Mechanics::Battle::Target->is_character($battle, $target);

	if ($at_character) {

		# TODO check if target is self / ally / foe
		return 1;
	}
	else {
		return $ability->target_ground;
	}
}

1;
