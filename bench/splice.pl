use header;

use Benchmark::Dumb qw(cmpthese);

my @arr_small = (1 .. 100);
my @arr_big = (1 .. 10000);

cmpthese 200.01, {
	'small push' => sub {
		push @arr_small, 1;
		pop @arr_small;
	},
	'small splice' => sub {
		splice @arr_small, 3, 0, 1;
		pop @arr_small;
	},
	'big push' => sub {
		push @arr_big, 1;
		pop @arr_big;
	},
	'big splice' => sub {
		splice @arr_big, 3, 0, 1;
		pop @arr_big;
	},
};

