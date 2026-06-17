##### NOTE ######
# These benchmarks run slower than in real env because of h2o, but can be
# useful to determine bottlenecks
#################

use Game::Mechanics::Check::Map qw(can_see);
use Game::Lore::Location;
use Game::Mechanics::Movement qw(move);
use Game::Object::Movement;
use Util::H2O;
use Utils;

use header;

use Benchmark::Dumb qw(cmpthese);

my $location = Game::Lore::Location->new(id => 'TEST', name => 'test', map => 'test_map');
my $map = $location->map;

my $variables = h2o -meth, {
	xy => sub ($self) { return ($self->pos_x, $self->pos_y) },
	pos_x => 4,
	pos_y => 3,
	set_pos_x => 3,
	set_pos_y => 3
};
my $actor = h2o {variables => $variables};

my $movement = Game::Object::Movement->new(
	variables => $variables,
	x => 7.3,
	y => 8.5,
	speed => 0.1,
	time => time,
);

cmpthese 200.01, {
	line_of_sight => sub {
		die unless can_see($map, [4.5, 3.8], [7.9, 8.3])->result;
	},
	movement => sub {
		die unless move($movement, $map, time);
	}
};

