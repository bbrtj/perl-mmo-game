package Game::Ability::EffectType;

use header;
use Moo::Role;
use Game::Common;

no header;

with 'Game::LoreElement';

sub get ($self, $type = undef)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::EffectType', 'EffectType/*.pm')};

	return defined $type ? $list->{$type} : $list;
}

1;
