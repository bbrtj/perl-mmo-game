package My::Moose;

use v5.32;
use warnings;

# use Hook::AfterRuntime;
use Import::Into;

require Mouse;

sub import
{
	my $caller = caller;
	my $self = shift;

	my %args;
	my @immutable_args;

	if (@_ > 0 && $_[0] eq -constr) {
		shift;
		%args = @_;
		push @immutable_args, inline_constructor => 0;
		push $args{-traits}->@*, 'My::Moose::Trait::LazyByDefault';
	}
	else {
		%args = @_;
	}

	Mouse->import::into($caller, %args);
	# after_runtime { $caller->meta->make_immutable(@immutable_args) };

	return;
}

1;
