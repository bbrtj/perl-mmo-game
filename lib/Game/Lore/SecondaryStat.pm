package Game::Lore::SecondaryStat;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'SSTA';

package Game::Lore::SecondaryStatData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

}

