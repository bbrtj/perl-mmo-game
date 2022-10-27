package My::Moose::Role;

use v5.36;

use Import::Into;

require Mouse::Role;
require Mooish::AttributeBuilder;

sub import ($me, %args)
{
	my $caller = caller;
	Mouse::Role->import::into($caller, %args);
	Mooish::AttributeBuilder->import::into($caller);

	return;
}

1;

