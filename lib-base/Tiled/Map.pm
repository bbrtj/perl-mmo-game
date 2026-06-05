package Tiled::Map;

use My::Moose;

use header;

use constant TILE_VOID => 1;
use constant TILE_WALL => 2;
use constant TILE_TERRAIN => 3;

has param 'path' => (
	isa => SimpleStr,
);

has param 'width' => (
	isa => PositiveInt,
);

has param 'height' => (
	isa => PositiveInt,
);

has param 'tilewidth' => (
	isa => PositiveInt,
);

has param 'tileheight' => (
	isa => PositiveInt,
);

has param 'map' => (
	isa => Str,
);

has field 'objects' => (
	constructed => ['Tiled::ObjectList', sub { shift->new(map => $_[0]) }],
);

