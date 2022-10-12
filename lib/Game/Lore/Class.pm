package Game::Lore::Class;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'CLAS';

package Game::Lore::ClassData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

}

