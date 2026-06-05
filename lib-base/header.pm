package header;

use v5.42;
use utf8;
use Import::Into;

require feature;
require builtin;

require i18n;
require Carp;
require Ref::Util;
require List::Util;
require List::Keywords;
require My::Serializer;
require ServerTime;

use DI;
use My::Dumper;

sub import ($me, @args)
{
	my $pkg = caller;

	strict->import::into($pkg);
	warnings->import::into($pkg);
	feature->unimport::out_of($pkg, ':all');
	feature->import::into($pkg, qw(:5.42 refaliasing declared_refs defer));
	builtin->import::into($pkg, qw(:5.42));
	utf8->import::into($pkg);
	Carp->import::into($pkg, qw(croak));
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
	warnings->unimport::out_of($pkg, 'experimental::refaliasing');
	warnings->unimport::out_of($pkg, 'experimental::declared_refs');
	warnings->unimport::out_of($pkg, 'experimental::defer');
	warnings->unimport::out_of($pkg, 'experimental::for_list');

	return;
}

