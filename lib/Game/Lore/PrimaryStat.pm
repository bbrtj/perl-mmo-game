package Game::Lore::PrimaryStat;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'PSTA';

package Game::Lore::PrimaryStatData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

}

