package My::Moose;

use v5.36;

use Hook::AfterRuntime;
use Import::Into;

require Moose;
require Sub::HandlesVia::Toolkit::Moose;

require namespace::autoclean;
require My::Mooish::AttributeBuilder;
require MooseX::XSAccessor;

sub common_traits ()
{
	return (
		'Sub::HandlesVia::Toolkit::Moose::PackageTrait'
	);
}

sub import ($self, @args)
{
	my $caller = caller;

	my %args;
	my @immutable_args;

	if (@args > 0 && $args[0] eq -constr) {
		%args = splice @args, 1;
		push @immutable_args, inline_constructor => 0;
		push $args{-traits}->@*, 'My::Moose::Trait::LazyByDefault';
	}
	else {
		%args = @args;
	}

	push @{$args{-traits}}, common_traits;

	Moose->import::into($caller, %args);
	namespace::autoclean->import(-cleanee => $caller);
	My::Mooish::AttributeBuilder->import::into($caller);
	MooseX::XSAccessor->import::into($caller);

	after_runtime { $caller->meta->make_immutable(@immutable_args) };

	return;
}

1;

