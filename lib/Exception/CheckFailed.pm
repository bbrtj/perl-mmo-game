package Exception::CheckFailed;

use Moo;

use header;

extends 'Exception';

has 'message' => (
	is => 'ro',
);

