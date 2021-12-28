package Repository::Role::Resource;

use My::Moose::Role;

use header;

requires qw(
	load
	save
);
