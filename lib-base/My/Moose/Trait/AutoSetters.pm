package My::Moose::Trait::AutoSetters;

use v5.42;
use My::Moose::Role;

around add_attribute => sub ($orig, $self, $name, @args) {
	my %params = @args == 1 ? $args[0]->%* : @args;

	if (exists $params{writer} && !$params{writer}) {
		delete $params{writer};
		return $self->$orig($name, %params);
	}

	# exit early if it's not something we want or can alter
	return $self->$orig($name, @args)
		if $name =~ /^_/
		|| $name =~ /^\+/;

	$params{writer} //= "set_$name";

	my $attribute = $self->$orig($name, %params);

	return $attribute;
};

