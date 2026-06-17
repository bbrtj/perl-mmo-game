package My::Moose::Trait::Attribute::Stored;

use v5.42;
use My::Moose::Role;

Moose::Util::meta_attribute_alias('Stored');

has param 'always_dirty' => (
	isa => Bool,
	default => false,
);

after install_accessors => sub ($self, @) {
	my $class = $self->associated_class;
	my $name = $self->name;

	if (!$self->always_dirty && $class->does_role('My::Moose::Role::TracksDirty')) {
		$class->add_after_method_modifier(
			$self->get_write_method,
			sub ($instance, $) {
				$instance->_dirty($name);
			}
		);
	}
};

