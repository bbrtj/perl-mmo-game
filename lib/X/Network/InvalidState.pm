package X::Network::InvalidState;

use My::Moose;

use header;

extends 'X::Network';

__END__

=pod

User game session state was not valid for the action user tried to perform
(like choosing the character before logging in).

