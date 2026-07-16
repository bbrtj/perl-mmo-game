use v5.42;
use experimental 'class';

class My::Game::TileMap::Pathfinding::Result;

field $path :reader :param;
field $target :reader :param;

method steps ()
{
	return $path->steps;
}

method step_count ()
{
	return $path->step_count;
}

method next_step ()
{
	return $path->next_step;
}

