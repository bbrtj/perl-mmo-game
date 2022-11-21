package My::Moose::Role;

use v5.36;

use Import::Into;
require My::Moose;

require Moose::Role;
require namespace::autoclean;
require My::Mooish::AttributeBuilder;

sub import ($me, %args)
{
	my $caller = caller;

	push @{$args{-traits}}, My::Moose::common_traits;

	My::Mooish::AttributeBuilder->import::into($caller);
	Moose::Role->import::into($caller, %args);

	# clean up the role so that unwanted stuff will not be composed
	namespace::autoclean->import(-cleanee => $caller);

	return;
}

1;

