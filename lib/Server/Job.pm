package Server::Job;

use My::Moose;
use DI;

use header;

with qw(
	Server::Processable
);

sub interval { ... }

