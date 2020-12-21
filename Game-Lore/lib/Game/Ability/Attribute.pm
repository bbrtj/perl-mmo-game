package Game::Ability::Attribute;

use header;
use Moo::Role;
use Game::Common;

no header;

with 'Game::LoreElement';

sub get ($self, $attribute = undef)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Attribute', 'Attribute/*.pm')};

	return defined $attribute ? $list->{$attribute} : $list;
}

1;
