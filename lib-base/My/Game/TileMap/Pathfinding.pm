package My::Game::TileMap::Pathfinding;

use v5.42;
use My::Game::TileMap::Pathfinding::Result;
use Game::TileMap::Pathfinding::Result;

use parent 'Game::TileMap::Pathfinding';

sub find_path ($self, @args)
{
	my $path = $self->_find_path(@args);
	return undef unless defined $path;

	$path->@* = map { $_ + 0.5 } $path->@*;
	return My::Game::TileMap::Pathfinding::Result->new(
		path => Game::TileMap::Pathfinding::Result->new($path),
		target => [@args[-2, -1]],
	);
}

