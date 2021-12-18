package Game::Item::Weapon;

use Moo::Role;
use Game::Common;

use header;

with 'Game::Ability';

requires qw(
	both_hands
	attribute
	range
	damage_deviation
	scaling
	base_damage_bonus
);

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Item::Weapon', 'Weapon/*.pm')};
}

