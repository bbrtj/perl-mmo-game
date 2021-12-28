package Game::Ability::EffectType;

use My::Moose::Role;
use Utils;

use header;

with 'Game::Lore';

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Ability::EffectType', 'EffectType/*.pm')};
}

