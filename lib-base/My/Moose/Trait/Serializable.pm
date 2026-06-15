package My::Moose::Trait::Serializable;

use v5.42;
use My::Moose::Role;

has field 'serialized_attributes' => (
	lazy => sub ($self) {
		return [grep { $_->does('My::Moose::Trait::Attribute::Stored') } $self->get_all_attributes];
	},
);

after initialize => sub ($self, $class, @args) {
	my $serialize_method = sub ($instance) {
		return {
			map {
				my $value = $_->get_value($instance);
				$_->name => defined $value ? "$value" : undef;
			} grep {
				$_->has_value($instance)
			} $instance->meta->serialized_attributes->@*
		};
	};

	$class->meta->add_method(serialize => $serialize_method);

	return;
};

