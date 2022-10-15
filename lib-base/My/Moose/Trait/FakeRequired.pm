package My::Moose::Trait::FakeRequired;

use v5.36;
use My::Moose::Role;
use Carp qw(croak);

has field 'required_attributes' => (
	lazy => sub { [] },
);

after initialize => sub ($self, $class, @args) {
	my $promote_method = sub ($instance) {
		my $meta = $instance->meta;

		for my $attr_name ($meta->required_attributes->@*) {
			my $attr = $meta->get_attribute($attr_name);
			croak "No value for $attr_name in " . (ref $instance) . ' (fake required)'
				unless $attr->has_value($instance);
		}

		return;
	};

	# promote method is for checking whether the model is complete
	# if the data comes from a source that is not trustworthy
	$class->meta->add_method(promote => $promote_method);

	my $buildargs_method = sub ($orig, $self, @args) {
		my $has_dummy;

		if (@args && $args[0] eq -dummy) {
			$has_dummy = 1;
			shift @args;
		}

		my %hash_args = @args == 1 ? $args[0]->%* : @args;
		$hash_args{__dummy} = $has_dummy;

		return $self->$orig(%hash_args);
	};

	$class->meta->add_around_method_modifier(BUILDARGS => $buildargs_method);

	my $build_method = sub ($self, $args) {
		unless ($args->{__dummy}) {
			$self->promote;
		}

		return;
	};

	if ($class->meta->has_method('BUILD')) {
		$class->meta->add_around_method_modifier(BUILD => $build_method);
	}
	else {
		$class->meta->add_method(BUILD => $build_method);
	}

	return;
};

around add_attribute => sub ($orig, $self, $name, @args) {
	my %params = @args == 1 ? $args[0]->%* : @args;

	if ($name !~ /^[_+]/ && $params{required}) {
		push $self->required_attributes->@*, $name;
		delete $params{required};
	}

	return $self->$orig($name, %params);
};

1;

