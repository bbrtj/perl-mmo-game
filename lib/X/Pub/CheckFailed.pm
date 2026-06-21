use v5.42;
use experimental 'class';

class X::Pub::CheckFailed :isa(X::Pub);

method _build_msg ()
{
	die 'msg is required for ' . __CLASS__;
}

