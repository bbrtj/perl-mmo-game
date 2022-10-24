package My::Moose::Role;

use v5.36;

use Import::Into;
use My::Sub::HandlesVia::Toolkit::Mouse::PackageTrait;

require Mouse::Role;
require Mooish::AttributeBuilder;

sub import ($me)
{
	my $caller = caller;
	Mouse::Role->import::into($caller, -traits => ['My::Sub::HandlesVia::Toolkit::Mouse::PackageTrait']);
	Mooish::AttributeBuilder->import::into($caller);

	return;
}

1;

