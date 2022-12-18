package Game::Object::Map;

use My::Moose;

use header;

extends 'Game::TileMap';

my $legend = __PACKAGE__->new_legend(characters_per_tile => 2)
	->add_wall('##')
	->add_void('@@')
	->add_terrain('__' => 'level_terrain')
	;

has extended 'legend' => (
	default => sub { $legend },
);

