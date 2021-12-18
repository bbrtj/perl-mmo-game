package Game::Character::Rating;

use Moo::Role;

use header;

with 'Game::Lore';

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Character::Rating', 'Rating/*.pm')};
}

