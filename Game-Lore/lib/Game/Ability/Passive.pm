package Game::Ability::Passive;

use header;
use Moo::Role;
use Game::Common;

no header;

with 'Game::Ability';

# TODO
requires qw(
);

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Passive', 'Passive/*.pm')};
}

1;

