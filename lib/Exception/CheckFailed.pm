package Exception::CheckFailed;

use My::Moose;

use header;

extends 'Exception';

has 'message' => (
	is => 'ro',
);

