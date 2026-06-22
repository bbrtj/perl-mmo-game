use experimental 'class';

class X::Pub::InvalidTarget :isa(X::Pub);

use header;

method _build_msg ()
{
	return Err::INVALID_TARGET;
}

