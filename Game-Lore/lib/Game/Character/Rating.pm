package Game::Character::Rating;

use header;
use Moo::Role;

no header;

with 'Game::LoreElement';

sub get ($self, $rating = undef)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Character::Rating', 'Rating/*.pm')};

	return defined $rating ? $list->{$rating} : $list;
}

1;
