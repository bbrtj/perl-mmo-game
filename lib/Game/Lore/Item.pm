package Game::Lore::Item;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'ITEM';

package Game::Lore::ItemData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

	has param 'type' => (
		writer => 1,
		isa => Types::Enum [qw(weapon armor consumable other)],
		default => 'other',
	);

}

