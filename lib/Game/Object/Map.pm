package Game::Object::Map;

use My::Moose;
use Tiled::Map;
use Tiled::Parser;
use Game::Object::Map::Spawn;
use Game::TileMap::Pathfinding;

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
	isa => InstanceOf ['Tiled::Map'],
	writer => -hidden,
	'handles->' => {
		'objects' => 'objects',
	},
);

has cached 'spawns' => (
	isa => ArrayRef [InstanceOf ['Game::Object::Map::Spawn']],
	lazy => 1,
);

has cached 'pathfinding' => (
	isa => InstanceOf ['Game::TileMap::Pathfinding'],
	lazy => sub ($self) {
		return Game::TileMap::Pathfinding->new(
			map => $self,
			diagonal_movement => true,
		);
	},
	'handles->' => {
		'find_path' => 'find_path',
	},
);

sub from_string ($self, $map_name)
{
	my $file_path = "locations/$map_name.tmx";
	state $parser = Tiled::Parser->new;
	$self->_set_map_object($parser->parse_map($file_path));

	return $self->SUPER::from_string($self->map_object->map);
}

sub _build_spawns ($self)
{
	return [
		map { Game::Object::Map::Spawn->new($_) }
			($self->objects->get_objects_of_type(Tiled::ObjectList->TYPE_SPAWNS) // [])->@*
	];
}

