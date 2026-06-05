package Component::Env;

use My::Moose;

use header;

# adjust any envvars here
my %defaults = (
	APP_MODE => 'development',
	DEBUG => false,
);

has param 'rawenv' => (
	isa => HashRef,
	default => sub {
		return {%defaults, %ENV};
	},
);

sub setenv ($self, $name, $value)
{
	$self->rawenv->{$name} = $value;

	return;
}

sub getenv ($self, $name)
{
	return $self->rawenv->{$name};
}

sub is_production ($self)
{
	return $self->rawenv->{APP_MODE} eq 'production';
}

