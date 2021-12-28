package Game::Ability::Passive;

use My::Moose::Role;
use Utils;

use header;

with 'Game::Ability';

# TODO
requires qw(
);

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Ability::Passive', 'Passive/*.pm')};
}

