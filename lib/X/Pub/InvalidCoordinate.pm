package X::Pub::InvalidCoordinate;

use My::Moose;

use header;

extends 'X';

sub build_msg
{
	return 'err.invalid_coordinate';
}

