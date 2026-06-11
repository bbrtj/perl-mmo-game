package My::Moose;

use v5.42;

use Hook::AfterRuntime;
use Import::Into;

require Moose;
require Sub::HandlesVia::Toolkit::Moose;

require namespace::autoclean;
require My::Mooish::AttributeBuilder;
require MooseX::XSAccessor;

use Types;

sub common_traits ()
{
	return (
		'Sub::HandlesVia::Toolkit::Moose::PackageTrait'
	);
}

sub import ($self, @args)
{
	my $caller = caller;

	my @immutable_args;
	my %flags = (-constr => 0, -strict => 0);
	%flags = (%flags, map { $_ => 1 } grep { exists $flags{$_} } @args);
	my %args = grep { !$flags{$_} } @args;

	if ($flags{-constr}) {
		push @immutable_args, inline_constructor => 0;
		push $args{-traits}->@*, 'My::Moose::Trait::LazyByDefault';
	}

	push @{$args{-traits}}, common_traits;

	Moose->import::into($caller, %args);
	namespace::autoclean->import(-cleanee => $caller, -except => qr{\A[A-Z_]{2,}\z});
	My::Mooish::AttributeBuilder->import::into($caller);
	MooseX::XSAccessor->import::into($caller);
	Types->import::into($caller, -types);

	if ($flags{-strict}) {
		MooseX::StrictConstructor->import::into($caller);
	}

	after_runtime { $caller->meta->make_immutable(@immutable_args) };

	return;
}

