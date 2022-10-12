package Game::Lore::Race;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'RACE';

package Game::Lore::RaceData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

}

