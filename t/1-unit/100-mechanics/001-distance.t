use testheader;

use Game::Mechanics::Distance qw(is_in_range);

test_data
	'should be in range' => [
		[[0, 0], [4, 3], 5.0],
		[[4, 3], [0, 0], 5.0],
	],
	'should be out of range' => [
		[[0, 0], [4, 3], 4.9],
	];

test should_be_in_range => sub {
	ok is_in_range(@_), $_;
};

test should_be_out_of_range => sub {
	ok !is_in_range(@_), $_;
};

done_testing;

