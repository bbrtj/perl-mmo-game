use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use Game::Mechanics::Check::Map;
use Game::Object::Map;
use Util::H2O;

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

my $location = h2o {map => $map};

cmpthese - 1, {
	line_of_sight => sub {
		die unless Game::Mechanics::Check::Map->can_see($location, [4.9, 4.8], [10.9, 9.3])->result;
	},
};

