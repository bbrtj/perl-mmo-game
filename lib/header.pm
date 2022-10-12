package header;

use v5.36;
use utf8;
use Import::Into;

require feature;
require true;

require i18n;
require Carp;
require Scalar::Util;
require Ref::Util;
require List::Util;

sub import ($me, @args)
{
	my $pkg = caller;

	feature->import::into($pkg, ':5.36', qw(try refaliasing declared_refs defer));
	feature->unimport::out_of($pkg, 'indirect');
	utf8->import::into($pkg);
	Carp->import::into($pkg, qw(croak));
	Scalar::Util->import::into($pkg, qw(blessed));
	Ref::Util->import::into($pkg, qw(is_arrayref is_hashref is_coderef));
	List::Util->import::into($pkg, qw(first any mesh));
	i18n->import::into($pkg);

	true->import::into($pkg);

	no_experimental_warnings($pkg);

	return;
}

# used rarely to get rid of experimental warnings after a module exported warnings
# must be used like this: BEGIN { header::no_experimental_warnings }
sub no_experimental_warnings ($pkg = caller)
{
	warnings->unimport::out_of($pkg, 'experimental::try');
	warnings->unimport::out_of($pkg, 'experimental::refaliasing');
	warnings->unimport::out_of($pkg, 'experimental::declared_refs');
	warnings->unimport::out_of($pkg, 'experimental::defer');
	warnings->unimport::out_of($pkg, 'experimental::for_list');

	return;
}

1;

