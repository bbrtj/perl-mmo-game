use testheader;

use Game::Mechanics::Check::Map;
use Game::Object::Map;
use Util::H2O;

test_data
	'should see' => [
		[[4.3, 1.2], [4.5, 1.4]],
		[[6.25, 6.33], [4.5, 4.4]],
		[[7.5, 4.6], [1.7, 10.3]],
		[[1.9, 9.06], [5.9, 6.7]],
	],
	'should not see' => [
		[[5.3, 1.2], [8.5, 1.4]],
		[[1.01, 9.3], [5.1, 6.2]],
		[[7.3, 4.4], [1.02, 10.3]],
		[[1.9, 9.06], [5.9, 6.1]],
	];

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

my $location = h2o { map => $map };

test should_see => sub {
	ok Game::Mechanics::Check::Map->can_see($location, @_)->result, $_;
};

test should_not_see => sub {
	ok Game::Mechanics::Check::Map->can_see($location, @_)->has_error, $_;
};

done_testing;

