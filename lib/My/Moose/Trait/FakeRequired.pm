package My::Moose::Trait::FakeRequired;

use v5.36;
use My::Moose::Role;

has 'required_attributes' => (
	is => 'ro',
	lazy => 1,
	default => sub { [] },
);

after initialize => sub {
	my ($self, $class, @args) = @_;

	my $promote_method = sub {
		my ($instance) = @_;
		my $meta = $instance->meta;

		for my $attr_name ($meta->required_attributes->@*) {
			my $attr = $meta->get_attribute($attr_name);
			die "No value for $attr_name in " . ref $instance
				unless $attr->has_value($instance);
		}

		return;
	};

	# check method is for checking whether the model is complete
	# if the data comes from a source that is not trustworthy
	$class->meta->add_method(check => $promote_method);

	return;
};

around add_attribute => sub {
	my ($orig, $self, $name, @args) = @_;
	my %params = @args == 1 ? $args[0]->%* : @args;

	if ($name !~ /^[_+]/ && $params{required}) {
		push $self->required_attributes->@*, $name;
		delete $params{required};
	}

	return $self->$orig($name, %params);
};

1;

