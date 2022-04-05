package Game::Lore::Area;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'AREA';

package Game::Lore::AreaData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

