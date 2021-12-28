package My::Moose::Trait::AutoSetters;

use v5.32;
use warnings;
use Moose::Role;

Moose::Util::meta_class_alias('AutoSetters');

around add_attribute => sub {
	my ($orig, $self, $name, @args) = @_;
	my %params = @args == 1 ? $args[0]->%* : @args;

	# exit early if it's not something we want or can alter
	return $self->$orig($name, @args)
		if (exists $params{writer} && !$params{writer})
		|| $name =~ /^_/
		|| $name =~ /^\+/;

	$params{writer} //= "set_$name";

	my $attribute = $self->$orig($name, %params);

	if ($self->does_role('My::Moose::Role::TracksDirty')) {
		$self->add_after_method_modifier(
			$attribute->get_write_method,
			sub {
				my ($instance, @args) = @_;
				$instance->_dirty->{$attribute->name} = 1;
			}
		);
	}

	return $attribute;
};

1;
