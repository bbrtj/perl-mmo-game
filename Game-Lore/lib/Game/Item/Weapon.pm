package Game::Item::Weapon;

use header;
use Moo::Role;
use Game::Common;

no header;

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
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Item::Weapon', 'Weapon/*.pm')};
}

1;

