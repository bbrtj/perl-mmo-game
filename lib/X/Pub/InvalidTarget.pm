use v5.42;
use experimental 'class';

class X::Pub::InvalidTarget :isa(X::Pub);

method _build_msg ()
{
	return 'err.invalid_target';
}

