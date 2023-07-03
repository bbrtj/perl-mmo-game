package My::Form::Tiny;

use v5.38;

use Import::Into;
require Form::Tiny;

sub import ($self, @args)
{
	my $pkg = caller;

	Form::Tiny->import::into($pkg, -nomoo, @args);

	return;
}

