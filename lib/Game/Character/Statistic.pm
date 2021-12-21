package Game::Character::Statistic;

use Moo::Role;

use header;

with 'Game::Lore';

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Character::Statistic', 'Statistic/*.pm')};
}

