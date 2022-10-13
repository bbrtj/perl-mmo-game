package DI;

use v5.36;

use Beam::Wire;
use Mooish::AttributeBuilder;
use Types::Standard qw(InstanceOf);

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

sub inject ($class, $name)
{
	my $config = $wire->get_config($name);

	return field $name => (
		isa => InstanceOf[$config->{class}],
		default => sub { $class->get($name) },
	);
}

sub has ($class, $name)
{
	return exists $wire->services->{$name};
}

1;

