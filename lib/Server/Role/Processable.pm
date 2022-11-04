package Server::Role::Processable;

use My::Moose::Role;

use header;

has injected 'models_repo';
has injected 'cache_repo';
has injected 'data_bus';

with qw(
	Server::Role::CanSendData
);

sub name { ... }
sub handle { ... }
use constant disabled => 0;

