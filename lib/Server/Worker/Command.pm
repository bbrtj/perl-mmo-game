package Server::Worker::Command;

use My::Moose::Role;

use header;

requires qw(
	name
	handler
);

