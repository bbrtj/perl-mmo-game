package My::Moose::Role;

use v5.36;

use Import::Into;
require My::Moose;

use constant TOOLKIT => My::Moose::TOOLKIT . '::Role';

eval 'require ' . TOOLKIT;
require namespace::autoclean;
require My::Mooish::AttributeBuilder;

sub import ($me, %args)
{
	my $caller = caller;

	push @{$args{-traits}}, 'Sub::HandlesVia::Toolkit::' . My::Moose::TOOLKIT . '::PackageTrait';

	My::Mooish::AttributeBuilder->import::into($caller);
	TOOLKIT()->import::into($caller, %args);

	# clean up the role so that unwanted stuff will not be composed
	# namespace::autoclean->import(-cleanee => $caller);

	return;
}

1;

