package My::Moose::Trait::LazyByDefault;

use v5.32;
use warnings;
use My::Moose::Role;

around add_attribute => sub {
	my ($orig, $self, $name, @args) = @_;
	my %params = @args == 1 ? $args[0]->%* : @args;

	# exit early if it's not something we can alter
	return $self->$orig($name, @args)
		if $name =~ /^\+/;

	if ($params{default} || $params{builder}) {
		$params{lazy} //= 1;
	}

	return $self->$orig($name, %params);
};

1;
