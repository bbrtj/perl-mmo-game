package Game::Lore::Race;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'RACE';

package Game::Lore::RaceData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

