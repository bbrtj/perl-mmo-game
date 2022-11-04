package Game::Lore::Attribute;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'ATTR';

package Game::Lore::AttributeData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

	has extended 'parent' => (
		isa => Types::InstanceOf ['Game::Lore::Attribute'],
	);
}

