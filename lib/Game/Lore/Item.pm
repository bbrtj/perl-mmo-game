package Game::Lore::Item;

use My::Moose;
use Types;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'ITEM';

package Game::Lore::ItemData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

	has 'type' => (
		is => 'ro',
		writer => 'set_type',
		isa => Types::Enum [qw(weapon armor consumable other)],
		default => sub { 'other' }
	);

}

