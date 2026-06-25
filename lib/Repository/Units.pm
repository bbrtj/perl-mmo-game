package Repository::Units;

use My::Moose;
use Sub::Util qw(set_subname);
use Utils qw(find_subclasses);

use header;

extends 'Repository';

has injected 'models_repo';

# load all factories
# introduce methods like: load_location, load_actor
BEGIN {
	my @factories = find_subclasses('Factory');

	foreach my $class (@factories) {
		my $factory_name = lc((split /::/, $class)[-1]);

		my $instance = $class->new;
		my $name = "load_${factory_name}";
		my $sub = sub ($obj, @params) {
			my $data = $instance->fetch(@params);
			return $instance->create($data);
		};

		no strict 'refs';    ## no critic 'TestingAndDebugging::ProhibitNoStrict'
		my $me = __PACKAGE__;
		*{"${me}::${name}"} = $sub;
		set_subname($sub, $name);
	}
}

sub save ($self, $unit, $update = 1)
{
	state $check = InstanceOf ['Unit'];
	$check->assert_valid($unit);

	my $models = $self->models_repo;
	$models->db->transaction(
		sub {
			$models->save($_, $update) for $unit->models->@*;
		}
	);
	return;
}

sub update ($self, $unit)
{
	return $self->save($unit, 1);
}

