package My::Moose;

use v5.36;

# use Hook::AfterRuntime;
use Import::Into;

require Mouse;    ## no critic 'Community::PreferredAlternatives'
require namespace::autoclean;

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

	Mouse->import::into($caller, %args);
	namespace::autoclean->import(-cleanee => $caller);

	# after_runtime { $caller->meta->make_immutable(@immutable_args) };

	return;
}

1;

