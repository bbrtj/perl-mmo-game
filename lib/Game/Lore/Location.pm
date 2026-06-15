package Game::Lore::Location;

use My::Moose;
use Game::Object::Map;

use header;

extends 'Game::Lore';

use constant prefix => 'loc';

has extended 'parent' => (
	isa => InstanceOf ['Game::Lore::Area'],
);

# pos_x / pos_y ON THE AREA MAP

has field 'pos_x' => (
	isa => Num,
	writer => 1,
);

has field 'pos_y' => (
	isa => Num,
	writer => 1,
);

has param 'map' => (
	coerce => (InstanceOf ['Game::Object::Map'])
		->plus_coercions(
			Str, q{ Game::Object::Map->new(map => $_) },
		),
);

has param 'connections' => (
	isa => ArrayRef [InstanceOf ['Game::Lore::Location']],
	default => sub { [] },
);

