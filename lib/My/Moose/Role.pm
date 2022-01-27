package My::Moose::Role;

use v5.32;
use warnings;

use Import::Into;

require Mouse::Role;

sub import
{
	my $caller = caller;
	Mouse::Role->import::into($caller);

	return;
}

1;
