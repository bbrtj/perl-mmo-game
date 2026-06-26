# HARNESS-DURATION-SHORT
use testheader;

use Game::Checks::Map qw(can_see);
use Game::Object::Map;

test_data
	'should see' => [
		[4.3, 1.2, 4.5, 1.4],
		[6.25, 6.33, 4.5, 4.4],
		[7.5, 4.6, 1.7, 10.3],
		[1.9, 9.06, 5.9, 6.7],
		[0.2, 0.1, 9.5, 1.1],
		[0.2, 0.1, 0.99, 9.5],
	],
	'should not see' => [
		[5.3, 1.2, 8.5, 1.4],
		[1.01, 9.3, 5.1, 6.2],
		[7.3, 4.4, 1.02, 10.3],
		[1.9, 9.06, 5.9, 6.1],
		[0.2, 0.2, 3.6, 3.3],
		[11.3, 0.2, 11.4, 10.5],
	];

my $map = Game::Object::Map->new(map => 'test_map');

test should_see => sub (@args) {
	ok lives { can_see($map, @args) }, $_;
};

test should_not_see => sub (@args) {
	ok dies { can_see($map, @args) }, $_;
};

done_testing;

