package DI;

use Beam::Wire;

use header;

my $wire = Beam::Wire->new(file => 'wire.yml');

sub get ($class, @args)
{
	return $wire->get(@args);
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
