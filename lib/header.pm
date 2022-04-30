package header;

use strict;
use warnings;
use utf8;
use feature ':5.32';
use Import::Into;
use List::Util qw(any);
use Hook::AfterRuntime;

use experimental;
require namespace::autoclean;
require true;

require i18n;
require Syntax::Keyword::Try;
require Carp;
require Scalar::Util;
require Safe::Isa;
require Ref::Util;

sub import
{
	my $pkg = caller;
	my ($me, @args) = @_;

	strict->import::into($pkg);
	warnings->import::into($pkg);
	utf8->import::into($pkg);
	feature->import::into($pkg, ':5.32', qw(isa signatures));
	Syntax::Keyword::Try->import::into($pkg);
	true->import::into($pkg);
	Carp->import::into($pkg, qw(croak));
	Scalar::Util->import::into($pkg, qw(blessed));
	Ref::Util->import::into($pkg, qw(is_arrayref is_hashref is_coderef));
	List::Util->import::into($pkg, qw(first any zip));
	Safe::Isa->import::into($pkg);
	i18n->import::into($pkg);

	namespace::autoclean->import(-cleanee => scalar(caller))
		unless any { $_ eq -noclean }
		@args;

	feature->unimport::out_of($pkg, 'indirect');
	no_experimental_warnings($pkg);

	return;
}

# used rarely to get rid of experimental warnings after a module exported warnings
# must be used like this: BEGIN { header::no_experimental_warnings }
sub no_experimental_warnings
{
	my ($pkg) = @_;
	$pkg //= caller;

	warnings->unimport::out_of($pkg, 'experimental::signatures');
	warnings->unimport::out_of($pkg, 'experimental::isa');

	return;
}

1;

