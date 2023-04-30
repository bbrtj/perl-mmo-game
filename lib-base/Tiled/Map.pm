package Tiled::Map;

use My::Moose;

use header;

use constant TILE_VOID => 1;
use constant TILE_WALL => 2;
use constant TILE_TERRAIN => 3;

has param 'width' => (
	isa => Types::PositiveInt,
);

has param 'height' => (
	isa => Types::PositiveInt,
);

has param 'tilewidth' => (
	isa => Types::PositiveInt,
);

has param 'tileheight' => (
	isa => Types::PositiveInt,
);

has param 'map' => (
	isa => Types::Str,
);

has field 'objects' => (
	constructed => ['Tiled::ObjectList', sub { shift->new(map => $_[0]) }],
);

