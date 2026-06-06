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

	has field 'speed_multiplier' => (
		isa => PositiveNum,
		writer => 1,
		default => 1,
	);

	has field 'damage_multiplier' => (
		isa => PositiveNum,
		writer => 1,
		default => 1,
	);

	has field 'projectile' => (
		isa =>
			Dict [speed => PositiveNum, range => PositiveNum, inaccuracy => PositiveOrZeroNum, radius => PositiveNum],
		writer => 1,
	);
}

