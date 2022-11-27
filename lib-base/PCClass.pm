package PCClass;

# this is an implementation of performance-critical classes. Such classes are
# constructed very often and must not use Moose because it will run at half a
# speed. Only base class features (constructor + accessors), no types or extra
# behavior.

use v5.36;

use Import::Into;
require Class::XSConstructor;
require Class::XSAccessor;

sub import ($self, @args)
{
	my $pkg = caller;
	Class::XSConstructor->import::into($pkg, @args);
	Class::XSAccessor->import::into($pkg, getters => [@args], setters => {map { +"set_$_" => $_ } @args});

	return;
}

1;

