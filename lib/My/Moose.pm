package My::Moose;

use v5.32;
use warnings;

use Hook::AfterRuntime;
use Import::Into;

require Moose;

sub import {
	my $caller = caller;
	my $self = shift;
	my @args = map {
		();
		inline_constructor => 0 if /-constr/;
	} @_;

	Moose->import::into($caller);
	after_runtime { $caller->meta->make_immutable(@args) };
}

1;
