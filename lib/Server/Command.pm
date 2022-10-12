package Server::Command;

use My::Moose;

use header;

with qw(
	Server::Processable
);

# stuff will be json-decoded if this is set to true
use constant deserializes => 0;

# should be reintroduced if a command uses any incoming data
sub validate ($self, $data)
{
	# clear data, since there's no need to pass around possible extra stuff
	return undef;
}

sub required_state { ... }

