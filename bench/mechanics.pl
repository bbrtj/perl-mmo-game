use lib 'lib-base';
use lib 'lib';
use Game::Mechanics::Check::Map;
use Game::Object::Map;
use Util::H2O;

use header;

use Benchmark qw(cmpthese);

my $map = Game::Object::Map->from_string(<<MAP);
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

my $location = h2o {map => $map};

cmpthese - 2, {
	line_of_sight => sub {
		Game::Mechanics::Check::Map->can_see($location, [7.5, 4.6], [1.7, 10.3]);
	},
};

