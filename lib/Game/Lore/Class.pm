package Game::Lore::Class;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'CLAS';

package Game::Lore::ClassData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

}

