package Game::Lore::Attribute;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'ATTR';

package Game::Lore::AttributeData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

	has 'subtype_of' => (
		is => 'ro',
		writer => 'set_subtype_of',
		isa => Types::InstanceOf['Game::Lore::Attribute'],
	);
}

