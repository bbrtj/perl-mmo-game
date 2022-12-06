package X::Pub::CheckFailed;

use My::Moose;

use header;

extends 'X';

has extended 'msg' => (
	required => 1
);

