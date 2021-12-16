package Game::Ability::EffectType;

use Moo::Role;
use Game::Common;

use header;

with 'Game::Lore';

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::EffectType', 'EffectType/*.pm')};
}

