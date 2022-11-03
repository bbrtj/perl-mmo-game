package Server::Event;

use My::Moose;

use header;

with qw(
	Server::Role::Processable
	Server::Role::WithGameProcess
);

