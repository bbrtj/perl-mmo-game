package MockObject;

use Moo;
use Object::Sub;

use header;

has 'mocked_subs' => (
	is => 'ro',
	default => sub { {} },
);

has 'object' => (
	is => 'ro',
	builder => 'setup',
	lazy => 1,
	clearer => '_clear_object',
);

sub setup ($self)
{
	my %methods = $self->mocked_subs->%*;
	my %init_hash;

	for my $method_name (keys %methods) {
		$init_hash{$method_name} = sub ($inner_self, @params) {
			my $params = $self->mocked_subs->{$method_name};
			$params->{called_times} += 1;
			push $params->{called_with}->@*, [@params];

			die $params->{throws}
				if defined $params->{throws};
			return $params->{returns};
		};
	}

	return Object::Sub->new(\%init_hash);
}

sub add_method ($self, $method, $returns = undef)
{
	$self->_clear_object
		unless exists $self->mocked_subs->{$method};

	$self->mocked_subs->{$method} = {
		called_times => 0,
		called_with => [],
		throws => undef,
		returns => $returns,
	};

	return $self->method($method);
}

# testers
sub method ($self, $method)
{
	my $params = $self->mocked_subs->{$method};
	croak "no method $method was mocked"
		unless defined $params;

	return Object::Sub->new({
		called_times => sub ($self) {
			return $params->{called_times};
		},
		called_with => sub ($self) {
			return $self->first_called_with;
		},
		first_called_with => sub ($self) {
			return $params->{called_with}[0];
		},
		last_called_with => sub ($self) {
			return $params->{called_with}[-1];
		},
		call_history => sub ($self) {
			return $params->{called_with}->@*;
		},
		should_return => sub ($self, $value) {
			$params->{returns} = $value;

			return $self->clear;
		},
		should_throw => sub ($self, $value) {
			$params->{throws} = $value;

			return $self->clear;
		},
		was_called => sub ($self) {
			return $self->called_times > 0;
		},
		was_called_once => sub ($self) {
			return $self->was_called_times(1);
		},
		was_called_times => sub ($self, $times) {
			return $self->called_times == $times;
		},
		clear => sub ($self) {
			$params->{called_times} = 0;
			$params->{called_with} = [];

			return $self;
		},
	});
}

