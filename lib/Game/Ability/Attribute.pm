package Game::Ability::Attribute;

use Moo::Role;
use Game::Common;

use header;

with 'Game::LoreElement';

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Attribute', 'Attribute/*.pm')};
}

