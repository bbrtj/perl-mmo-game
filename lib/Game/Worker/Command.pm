package Game::Worker::Command;

use Moo::Role;

use header;

requires qw(
	name
	handler
);

