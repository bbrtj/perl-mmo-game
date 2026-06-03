package Game::Lore::Ability;

use My::Moose;

use header;

## no critic 'Modules::ProhibitMultiplePackages'

extends 'Game::Lore';

use constant prefix => 'ABIL';

package Game::Lore::AbilityData {
	use My::Moose;

	use header;

	extends 'Game::LoreData';

	has field 'weapon_based' => (
		isa => Types::Bool,
		writer => 1,
	);

	has field 'speed_multiplier' => (
		isa => Types::PositiveNum,
		writer => 1,
	);
}

