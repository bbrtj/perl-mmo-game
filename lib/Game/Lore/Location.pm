package Game::Lore::Location;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'LOC';


package Game::Lore::LocationData {
	use My::Moose;
	use Types;
	use Game::Object::Map;

	use header;

	extends 'Game::LoreData';

	has 'pos_x' => (
		is => 'ro',
		writer => 'set_pos_x',
		isa => Types::Num,
	);

	has 'pos_y' => (
		is => 'ro',
		writer => 'set_pos_y',
		isa => Types::Num,
	);

	has 'map' => (
		is => 'ro',
		writer => '_set_map',
		isa => Types::InstanceOf['Game::Object::Map'],
	);

	has 'connections' => (
		is => 'ro',
		default => sub { [] },
		isa => Types::ArrayRef [Types::InstanceOf ['Game::Lore::Location']],
	);

	has '+parent' => (
		isa => Types::InstanceOf['Game::Lore::Area'],
	);

	sub set_map ($self, $map_str)
	{
		$self->_set_map(Game::Object::Map->from_string($map_str));
		return;
	}
}

