##### NOTE ######
# These benchmarks run slower than in real env, but can be useful to determine
# bottlenecks
#################

use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use Game::Mechanics::Check::Map;
use Game::Object::Map;
use Game::Mechanics::Movement;
use Game::Object::Movement;
use Util::H2O;
use Time::HiRes qw(time);
use Utils;

use header;

use Benchmark qw(cmpthese);

my $map = Game::Object::Map->new(map => <<MAP);
............
.OOOO..OOOO.
.OOOO..OOOO.
.O..OO...OO.
.O..OOOOOOO.
.OO..OOOOOO.
.O...OO.....
...OOOOOO...
..OOOOOOOO..
.OOO....OOO.
.OO......OO.
............
MAP

my $variables = h2o {pos_x => 3, pos_y => 3, set_pos_x => 3, set_pos_y => 3};
my $location = h2o {map => $map};
my $actor = h2o {variables => $variables};

my $movement;

# my $base = $movement->time;
# my $elapsed = $base + 0.5;

cmpthese - 2, {
	line_of_sight => sub {
		die unless Game::Mechanics::Check::Map->can_see($location, [4.9, 4.8], [10.9, 9.3])->result;
	},
	movement => sub {
		$movement //= Game::Object::Movement->new(
			variables => $variables,
			x => 5.3,
			y => 6.5,
			speed => 1,
			time => time,
		);

		# note: this will start dying at 4 seconds benchmark
		die unless Game::Mechanics::Movement->move($movement, time, $map);
	}
};

