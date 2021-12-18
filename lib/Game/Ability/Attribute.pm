package Game::Ability::Attribute;

use Moo::Role;
use Game::Common;

use header;

with 'Game::Lore';

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Attribute', 'Attribute/*.pm')};
}

