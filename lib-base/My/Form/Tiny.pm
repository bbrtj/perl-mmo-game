package My::Form::Tiny;

use v5.36;

use Import::Into;
require Form::Tiny;

sub import ($self, @args)
{
	my $pkg = caller;

	Form::Tiny->import::into($pkg, -nomoo, @args);
}

1;

