use v5.42;
use experimental 'class';

class X::Pub :isa(X);

# in this context, this is a string id to be translated
sub _build_msg
{
	return 'err';
}

