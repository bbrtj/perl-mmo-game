package Game::Lore::Area;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'AREA';

package Game::Lore::AreaData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

}

