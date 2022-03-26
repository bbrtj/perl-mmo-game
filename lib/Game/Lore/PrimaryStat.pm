package Game::Lore::PrimaryStat;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'PSTA';

package Game::Lore::PrimaryStatData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

