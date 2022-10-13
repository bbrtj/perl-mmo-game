package MockObject;

use My::Moose;
use Util::H2O;
use MockObject::Method;

use header;

has option 'context' => (
	writer => 1,
);

has field 'mocked_subs' => (
	default => sub { {} },
);

has field 'object' => (
	lazy => 'setup',
	clearer => -hidden
);

sub setup ($self)
{
	my %methods = $self->mocked_subs->%*;
	my %init_hash;

	for my $method_name (keys %methods) {
		my $method = $self->mocked_subs->{$method_name};
		$init_hash{$method_name} = sub ($inner_self, @params) {
			return $method->_called($inner_self, @params);
		};
	}

	return h2o -meth, \%init_hash;
}

sub add_method ($self, $method_name, @returns)
{
	$self->_clear_object;
	my $method = $self->mocked_subs->{$method_name} = MockObject::Method->new;

	if (@returns) {
		$method->should_return(@returns);
	}

	return $method;
}

sub method ($self, $method_name)
{
	return $self->mocked_subs->{$method_name}
		// croak "Method $method_name was not mocked!";
}

sub m ($self, $method_name = $self->context)
{
	croak 'No context was set'
		unless defined $method_name || $self->has_context;

	return $self->method($method_name);
}

