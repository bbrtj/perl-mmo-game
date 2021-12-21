package Game::Item::ArmorType;

use Moo::Role;
use Utils;

use header;

with 'Game::Ability';

requires qw(
	endurance
	willpower
);

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Item::ArmorType', 'ArmorType/*.pm')};
}

