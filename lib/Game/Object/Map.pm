package Game::Object::Map;

use My::Moose;

use header;

extends 'Game::TileMap';

my $legend = __PACKAGE__->new_legend
	->add_wall('.')
	->add_void('X')
	->add_terrain('O' => 'map')
	;

has extended 'legend' => (
	default => sub { $legend },
);

