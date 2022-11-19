package My::Moose;

use v5.36;

use Hook::AfterRuntime;
use Import::Into;
use Module::Load qw(load);

# Moose or mouse
use constant TOOLKIT => 'Moose';

load TOOLKIT;
load 'Sub::HandlesVia::Toolkit::' . TOOLKIT;

require namespace::autoclean;
require My::Mooish::AttributeBuilder;
require MooseX::XSAccessor;

sub common_traits ()
{
	return (
		'Sub::HandlesVia::Toolkit::' . TOOLKIT . '::PackageTrait'
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

	TOOLKIT()->import::into($caller, %args);
	namespace::autoclean->import(-cleanee => $caller);
	My::Mooish::AttributeBuilder->import::into($caller);

	# for Moose, make immutable
	if (TOOLKIT eq 'Moose') {
		MooseX::XSAccessor->import::into($caller);
		after_runtime { $caller->meta->make_immutable(@immutable_args) };
	}

	return;
}

1;

