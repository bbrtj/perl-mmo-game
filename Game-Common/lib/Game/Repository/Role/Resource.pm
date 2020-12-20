package Game::Repository::Role::Resource;

use header;
use Moo::Role;

no header;

requires qw(
	load
	save
);

1;
