package MockObject;

use My::Moose;
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

			die $params->{returns}
				if $params->{throws};

			return $params->{returns}->@*
				if $params->{returns_list} && ref $params->{returns} eq 'ARRAY';

			return $params->{returns}->(@params)
				if $params->{calls_return} && ref $params->{returns} eq 'CODE';

			return $params->{returns};
		};
	}

	return Object::Sub->new(\%init_hash);
}

sub add_method ($self, $method, @returns)
{
	$self->_clear_object
		unless exists $self->mocked_subs->{$method};

	$self->mocked_subs->{$method} = {
		called_times => 0,
		called_with => [],
		throws => 0,
		calls_return => 0,
	};

	return $self->method($method)->should_return(@returns);
}

# testers
sub method ($self, $method)
{
	my $params = $self->mocked_subs->{$method};
	croak "no method $method was mocked"
		unless defined $params;

	return Object::Sub->new(
		{
			called_times => sub ($self) {
				return $params->{called_times};
			},

			called_with => sub ($self) {
				return $self->last_called_with;
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

			should_return => sub ($self, @values) {
				$params->{throws} = 0;
				$params->{calls_return} = 0;

				if (@values == 1) {
					$params->{returns} = $values[0];
					$params->{returns_list} = 0;
				}
				else {
					$params->{returns} = [@values];
					$params->{returns_list} = 1;
				}

				return $self->clear;
			},

			should_call => sub ($self, $sub) {
				$params->{throws} = 0;
				$params->{calls_return} = 1;
				$params->{returns} = $sub;

				return $self->clear;
			},

			should_throw => sub ($self, $exception) {
				$params->{throws} = 1;
				$params->{returns} = $exception;

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
		}
	);
}

