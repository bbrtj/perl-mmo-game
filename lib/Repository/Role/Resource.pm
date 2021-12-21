package Repository::Role::Resource;

use Moo::Role;

use header;

requires qw(
	load
	save
);
