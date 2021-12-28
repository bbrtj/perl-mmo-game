package My::Moose::Role;

use v5.32;
use warnings;

use Import::Into;

require Moose::Role;

sub import
{
	my $caller = caller;
	Moose::Role->import::into($caller);
}

1;
