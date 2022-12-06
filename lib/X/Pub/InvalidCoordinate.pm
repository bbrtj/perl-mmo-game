package X::Pub::InvalidCoordinate;

use My::Moose;

use header;

extends 'X';

sub build_msg { 'err.invalid_coordinate' }

