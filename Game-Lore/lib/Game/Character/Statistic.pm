package Game::Character::Statistic;

use header;
use Moo::Role;

no header;

with 'Game::LoreElement';

sub get ($self, $statistic = undef)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Character::Statistic', 'Statistic/*.pm')};

	return defined $statistic ? $list->{$statistic} : $list;
}

1;
