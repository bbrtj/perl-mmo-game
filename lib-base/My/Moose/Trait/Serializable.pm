package My::Moose::Trait::Serializable;

use v5.38;
use My::Moose::Role;

has field 'serialized_attributes' => (
	lazy => sub ($self) {
		return [grep { $_->name !~ /^_/ } $self->get_all_attributes];
	},
);

after initialize => sub ($self, $class, @args) {
	my $serialize_method = sub ($instance) {
		return {
			map {
				$_->name => $_->get_value($instance)
			} grep {
				$_->has_value($instance)
			} $instance->meta->serialized_attributes->@*
		};
	};

	$class->meta->add_method(serialize => $serialize_method);

	return;
};

