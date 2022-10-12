package DI;

use v5.36;

use Beam::Wire;

my $wire = Beam::Wire->new(file => 'wire.yml');

sub get ($class, $name, %args)
{
	%args = (args => {%args})
		if keys %args;
	return $wire->get($name, %args);
}

sub set ($class, $name, $value, $replace = 0)
{
	if ($replace || !exists $wire->services->{$name}) {
		$wire->set($name, $value);
	}
	return;
}

sub forget ($class, $name)
{
	if (exists $wire->services->{$name}) {
		delete $wire->services->{$name};
	}
	return;
}

sub has ($class, $name)
{
	return exists $wire->services->{$name};
}

1;

