package Game::Item::Weapon;

use My::Moose::Role;
use Utils;

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
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Item::Weapon', 'Weapon/*.pm')};
}

