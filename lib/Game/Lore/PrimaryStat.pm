package Game::Lore::PrimaryStat;

use My::Moose;
use Types;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'PSTA';

package Game::Lore::PrimaryStatData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

