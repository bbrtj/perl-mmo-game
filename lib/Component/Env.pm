package Component::Env;

use My::Moose;
use Env::Dot;

use header;

# adjust any envvars here
my %defaults = (
	APP_MODE => 'development',
);

has param 'rawenv' => (
	isa => Types::HashRef,
	default => sub {
		return {%defaults};
	},
);

sub setenv ($self, $name, $value)
{
	$self->rawenv->{$name} = $value;

	return;
}

sub getenv ($self, $name)
{
	my $rawenv = $self->rawenv;

	my $value = exists $rawenv->{$name}
		? $rawenv->{$name}
		: exists $ENV{$name}
		? $ENV{$name}
		: croak "unknown environmental variable $name"
		;

	return $value;
}

sub is_production ($self)
{
	return $self->rawenv->{APP_MODE} eq 'deployment'
		|| $self->rawenv->{APP_MODE} eq 'production';
}

