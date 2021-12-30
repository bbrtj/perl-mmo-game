package Exception;

use My::Moose;
use Types;

use header;

with 'Throwable';

# TODO: stringify with the msg if present?
has 'msg' => (
	is => 'ro',
	isa => Types::Str,
);

