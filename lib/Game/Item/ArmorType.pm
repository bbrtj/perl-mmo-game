package Game::Item::ArmorType;

use Moo::Role;
use Game::Common;

use header;

with 'Game::Ability';

requires qw(
	endurance
	willpower
);

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Item::ArmorType', 'ArmorType/*.pm')};
}

