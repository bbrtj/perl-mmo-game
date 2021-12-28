package Component::Env;

use Types;
use My::Moose;
use Dotenv -load;

use header;

# adjust any envvars here
my %defaults = (
	APP_MODE => 'development',
);

has 'rawenv' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub {
		return {%defaults, %ENV};
	},
);

sub getenv : lvalue ($self, $name)
{
	my $rawenv = $self->rawenv;

	croak "unknown environmental variable $name"
		unless exists $rawenv->{$name};

	return $rawenv->{$name};
}
