package Test::Spy;

use My::Moose;
use Util::H2O;
use Test::Spy::Method;

use header;

has field 'mocked_subs' => (
	default => sub { {} },
);

has field 'object' => (
	lazy => 'setup',
	clearer => -hidden
);

with qw(Test::Spy::Facade);

sub _no_method ($self, $method_name)
{
	croak "method $method_name was not mocked!";
}

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
	my $method = $self->mocked_subs->{$method_name} = Test::Spy::Method->new(method_name => $method_name);

	if (@returns) {
		$method->should_return(@returns);
	}

	return $method;
}

sub method ($self, $method_name)
{
	return $self->mocked_subs->{$method_name}
		// $self->_no_method($method_name);
}

around _get_context => sub ($orig, $self, @args) {
	my $context_method = $self->$orig(@args);

	return $self->mocked_subs->{$context_method}
		// $self->_no_method($context_method);
};

