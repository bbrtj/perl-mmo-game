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

	# pos_x / pos_y ON THE AREA MAP

	has option 'pos_x' => (
		writer => 1,
		isa => Num,
	);

	has option 'pos_y' => (
		writer => 1,
		isa => Num,
	);

	has option 'map' => (
		writer => -hidden,
		isa => InstanceOf ['Game::Object::Map'],
	);

	has param 'connections' => (
		isa => ArrayRef [InstanceOf ['Game::Lore::Location']],
		default => sub { [] },
	);

	has extended 'parent' => (
		isa => InstanceOf ['Game::Lore::Area'],
	);

	sub set_map ($self, $map_str)
	{
		$self->_set_map(Game::Object::Map->new(map => $map_str));
		return;
	}

}

