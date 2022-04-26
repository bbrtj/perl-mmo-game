package Repository::Units;

use My::Moose;
use Types;
use Mojo::Loader qw(load_classes);
use Sub::Util qw(set_subname);

use header;

# load all factories
# introduce methods like: get_location, get_actor
BEGIN {
	my @factories = load_classes('Factory');

	for my $class (@factories) {
		my $factory_name = lc ((split/::/, $class)[-1]);

		my $instance = $class->new;
		my $name = "get_${factory_name}";
		my $sub = sub ($obj, @params) {
			my $data = $instance->fetch(@params);
			return $instance->create($data);
		};

		no strict 'refs';
		my $me = __PACKAGE__;
		*{"${me}::${name}"} = $sub;
		set_subname($sub, $name);
	}
}

has 'repo' => (
	is => 'ro',
);

sub save ($self, $unit, $update = 1)
{
	state $check = Types::InstanceOf ['Unit'];
	$check->assert_valid($unit);

	$self->repo->save($_, $update) for $unit->models->@*;
	return;
}

