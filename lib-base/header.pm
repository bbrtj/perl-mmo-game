package header;

use v5.38;
use utf8;
use Import::Into;

require feature;

require i18n;
require Carp;
require Scalar::Util;
require Ref::Util;
require List::Util;
require List::Keywords;
require My::Serializer;
require ServerTime;

use DI;
use Types;
use My::Dumper;

sub import ($me, @args)
{
	my $pkg = caller;

	strict->import::into($pkg);
	warnings->import::into($pkg);
	feature->unimport::out_of($pkg, ':all');
	feature->import::into($pkg, qw(:5.38 try refaliasing declared_refs defer));
	utf8->import::into($pkg);
	Carp->import::into($pkg, qw(croak));
	Scalar::Util->import::into($pkg, qw(blessed));
	Ref::Util->import::into($pkg, qw(is_ref is_arrayref is_hashref is_coderef is_scalarref));
	List::Keywords->import::into($pkg, qw(first any all));
	List::Util->import::into($pkg, qw(mesh));
	ServerTime->import::into($pkg, qw(server_time));
	i18n->import::into($pkg);
	My::Serializer->import::into($pkg);

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

