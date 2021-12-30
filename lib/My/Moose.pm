package My::Moose;

use v5.32;
use warnings;

use Hook::AfterRuntime;
use Import::Into;

use My::Moose::Trait::AutoSetters;
use My::Moose::Trait::Serializable;

require Moose;

sub import
{
	my $caller = caller;
	my $self = shift;

	my @args = grep {
		$_ ne -constr
	} @_;

	my @immutable_args = @args < @_
		? (inline_constructor => 0)
		: ();

	Moose->import::into($caller, @args);
	after_runtime { $caller->meta->make_immutable(@immutable_args) };

	return;
}

1;
