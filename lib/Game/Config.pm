package Game::Config;

use header;

# required for the define DSL to work
sub define
{
	state $defined = {
		supported_langs => [qw(pl)],
	};
	return $defined;
}

sub config
{
	return define;
}

