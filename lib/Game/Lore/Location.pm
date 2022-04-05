package Game::Lore::Location;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'LOC';


package Game::Lore::LocationData {
	use My::Moose;
	use Types;

	use header;

	extends 'Game::LoreData';

	has 'x' => (
		is => 'ro',
		writer => 'set_x',
		isa => Types::Num,
	);

	has 'y' => (
		is => 'ro',
		writer => 'set_y',
		isa => Types::Num,
	);

	has 'connections' => (
		is => 'ro',
		default => sub { [] },
	);

	has '+parent' => (
		isa => Types::InstanceOf['Game::Lore::Area'],
	);

}

