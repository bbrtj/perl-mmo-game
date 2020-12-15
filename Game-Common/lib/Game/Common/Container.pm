package Game::Common::Container;

use Mojo::Base -signatures;
use Exporter qw(import);
use Carp;

our @EXPORT = qw(
	resolve
);

our @EXPORT_OK = qw (
	set_container
	add_to_container
);

my %container = ();

sub resolve($key)
{
	croak "unknown dependency $key" unless exists $container{$key};
	return $container{$key};
}

sub set_container(%new)
{
	%container = %new;
	return;
}

sub add_to_container($key, $value)
{
	$container{$key} = $value;
	return;
}


1;

