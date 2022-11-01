package My::Moose::Role;

use v5.36;

use Import::Into;

require Mouse::Role;
require namespace::autoclean;
require My::Mooish::AttributeBuilder;

sub import ($me, %args)
{
	my $caller = caller;

	My::Mooish::AttributeBuilder->import::into($caller);
	Mouse::Role->import::into($caller, %args);

	# clean up the role so that unwanted stuff will not be composed
	namespace::autoclean->import(-cleanee => $caller);

	return;
}

1;

