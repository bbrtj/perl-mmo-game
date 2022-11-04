package Game::Lore::Location;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'LOC';

package Game::Lore::LocationData {
	use My::Moose;
	use Game::Object::Map;

	use header;

	extends 'Game::LoreData';

	has option 'pos_x' => (
		writer => 1,
		isa => Types::Num,
	);

	has option 'pos_y' => (
		writer => 1,
		isa => Types::Num,
	);

	has option 'map' => (
		writer => -hidden,
		isa => Types::InstanceOf ['Game::Object::Map'],
	);

	has param 'connections' => (
		isa => Types::ArrayRef [Types::InstanceOf ['Game::Lore::Location']],
		default => sub { [] },
	);

	has extended 'parent' => (
		isa => Types::InstanceOf ['Game::Lore::Area'],
	);

	sub set_map ($self, $map_str)
	{
		$self->_set_map(Game::Object::Map->from_string($map_str));
		return;
	}
}

