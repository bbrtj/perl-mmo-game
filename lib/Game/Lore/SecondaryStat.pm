package Game::Lore::SecondaryStat;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'SSTA';

package Game::Lore::SecondaryStatData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

