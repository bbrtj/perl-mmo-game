package Game::Mechanics::Check::Ability;

use header;
use Game::Types qw(InstanceOf);
use Game::Mechanics::Check::Map;
use Game::Mechanics::Distance;
use Game::Ability;
use Game::Mechanics::Check::Carry;

use constant BATTLE_TYPE => InstanceOf ['Game::Unit::Battle'];

no header;

sub in_range ($self, $battle, $character_id, $ability_id, @position)
{
	BATTLE_TYPE->assert_valid($battle);
	my $ability = Game::Ability->get($ability_id);
	my $range = $ability->range // 5;    # TODO weapon range
	my $contestant = $battle->find_contestant($character_id);
	my @pos_cur = ($contestant->[0]->pos_x, $contestant->[0]->pos_y);

	return Game::Mechanics::Check::Carry->gather(
		'out_of_range',
		Game::Mechanics::Distance->is_in_range(\@pos_cur, \@position, $range),
		Game::Mechanics::Check::Map->can_see($battle, @position)
	);
}

1;
