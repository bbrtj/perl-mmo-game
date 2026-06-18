package DI;

use v5.42;

use Beam::Wire;
use Types::Standard qw(InstanceOf);
use Sub::Install;

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

sub injected ($class, $name)
{
	my $config = $wire->get_config($name);

	return (
		isa => InstanceOf [$config->{class}],
		default => sub { $class->get($name) },
	);
}

sub static_injected ($class, $name)
{
	my $caller = caller;
	my $code = sub {
		state $service = $class->get($name);
		return $service;
	};

	if ($caller->can('meta')) {
		$caller->meta->add_method($name, $code);
	}
	else {
		Sub::Install::install_sub(
			{
				code => $code,
				as => $name,
				into => $caller,
			}
		);
	}
}

sub has ($class, $name)
{
	return exists $wire->services->{$name};
}

