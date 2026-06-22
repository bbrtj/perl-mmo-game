use experimental 'class';

class X::Pub::InvalidCoordinate :isa(X::Pub);

use header;

method _build_msg ()
{
	return Err::INVALID_COORDINATE;
}

