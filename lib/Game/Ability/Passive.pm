package Game::Ability::Passive;

use Moo::Role;
use Game::Common;

use header;

with 'Game::Ability';

# TODO
requires qw(
);

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Passive', 'Passive/*.pm')};
}

