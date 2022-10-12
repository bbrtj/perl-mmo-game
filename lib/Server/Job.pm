package Server::Job;

use My::Moose;

use header;

with qw(
	Server::Processable
);

sub interval { ... }

