package X::Pub::InvalidTarget;

use My::Moose;

use header;

extends 'X';

sub build_msg
{
	return 'err.invalid_target';
}

