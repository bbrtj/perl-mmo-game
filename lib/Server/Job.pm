package Server::Job;

use My::Moose;

use header;

with qw(
	Server::Role::Processable
);

sub interval { ... }

