package My::Moose::Trait::AutoSetters;

use v5.38;
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

	if ($self->does_role('My::Moose::Role::TracksDirty')) {
		$self->add_after_method_modifier(
			$attribute->get_write_method,
			sub ($instance, $new_value) {
				$instance->_dirty($attribute->name);
			}
		);
	}

	return $attribute;
};

