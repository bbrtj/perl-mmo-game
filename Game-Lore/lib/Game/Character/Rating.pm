package Game::Character::Rating;

use header;
use Moo::Role;

no header;

with 'Game::LoreElement';

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Character::Rating', 'Rating/*.pm')};
}

1;
