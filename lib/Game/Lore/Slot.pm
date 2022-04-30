package Game::Lore::Slot;

use My::Moose;
use Types;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'SLOT';

package Game::Lore::SlotData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

