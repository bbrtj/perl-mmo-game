package Game::Object::Map;

use My::Moose;
use Tiled::Map;
use Tiled::Parser;

use header;

extends 'Game::TileMap';

my $legend = __PACKAGE__->new_legend(characters_per_tile => 1)
	->add_wall(Tiled::Map::TILE_WALL)
	->add_void(Tiled::Map::TILE_VOID)
	->add_terrain(Tiled::Map::TILE_TERRAIN => 'terrain')
	;

has extended 'legend' => (
	default => sub { $legend },
);

has field 'map_object' => (
	isa => Types::InstanceOf['Tiled::Map'],
	writer => -hidden,
	'handles{}' => {
		'objects' => 'objects',
	},
);

sub from_string ($self, $map_name)
{
	my $file_path = "locations/$map_name.tmx";
	state $parser = Tiled::Parser->new;
	$self->_set_map_object($parser->parse_map($file_path));

	return $self->SUPER::from_string($self->map_object->map);
}

