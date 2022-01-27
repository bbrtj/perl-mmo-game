package My::Moose::Trait::Serializable;

use v5.32;
use warnings;
use My::Moose::Role;

has 'serialized_attributes' => (
	is => 'ro',
	lazy => 1,
	default => sub {
		return [grep { $_->name !~ /^_/ } shift->get_all_attributes];
	},
);

after initialize => sub {
	my ($self, $class, @args) = @_;

	my $serialize_method = sub {
		my ($instance) = @_;

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

1;
