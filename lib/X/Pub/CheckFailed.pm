use experimental 'class';

class X::Pub::CheckFailed :isa(X::Pub);

use header;

method _build_msg ()
{
	die 'msg is required for ' . __CLASS__;
}

