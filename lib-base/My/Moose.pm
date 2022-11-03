package My::Moose;

use v5.36;

use Hook::AfterRuntime;
use Import::Into;

use constant TOOLKIT => 'Mouse';

eval 'require ' . TOOLKIT;
eval 'require Sub::HandlesVia::Toolkit::' . TOOLKIT;

require namespace::autoclean;
require My::Mooish::AttributeBuilder;

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

	push @{$args{-traits}}, 'Sub::HandlesVia::Toolkit::' . TOOLKIT . '::PackageTrait';

	TOOLKIT()->import::into($caller, %args);
	namespace::autoclean->import(-cleanee => $caller);
	My::Mooish::AttributeBuilder->import::into($caller);

	if (TOOLKIT eq 'Moose') {
		after_runtime { $caller->meta->make_immutable(@immutable_args) };
	}

	return;
}

1;

