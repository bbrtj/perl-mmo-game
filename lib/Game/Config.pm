package Game::Config;

use header;

# required for the define DSL to work
sub define
{
	state $defined = {};
	return $defined;
}

sub config
{
	return define;
}

