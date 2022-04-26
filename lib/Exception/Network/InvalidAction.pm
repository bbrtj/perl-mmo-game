package Exception::Network::InvalidAction;

use My::Moose;

use header;

extends 'Exception::Network';

__END__

=pod

User action was not present or not recognized.

