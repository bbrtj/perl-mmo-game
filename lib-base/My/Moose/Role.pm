package My::Moose::Role;

use v5.36;

use Import::Into;

require Mouse::Role;
require Mooish::AttributeBuilder;

sub import ($me)
{
	my $caller = caller;
	Mouse::Role->import::into($caller);
	Mooish::AttributeBuilder->import::into($caller);

	return;
}

1;

