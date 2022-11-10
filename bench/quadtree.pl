use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use Algorithm::QuadTree;

use header;

use Benchmark qw(cmpthese);

my $aqt_predeclared = Algorithm::QuadTree->new(
	-depth => 8,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

# depth = 8 will divide each dimension by 2^8 (256), so this quadtree will be
# accurate to 100 / 256 = at least 0.5 unit
my $aqt_big = Algorithm::QuadTree->new(
	-depth => 8,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

# if we want to keep this precision, we have to have depth big enough so that
# bigger dimension is at least two times smaller than depth ^ 2
# (for example for 15/30: 30 * 2 = 60, next power of 2 is 64 (2 ^ 6))
my $aqt_small = Algorithm::QuadTree->new(
	-depth => 6,
	-xmin => 0,
	-ymin => 0,
	-xmax => 15,
	-ymax => 30,
);

my $obj = 'obj';

foreach my $i (1 .. 1000) {
	$aqt_predeclared->add($obj, rand 100, rand 100, 0.5);
}

cmpthese -5, {
	'big clear + insert 1000' => sub {
		$aqt_big->clear;
		foreach my $i (1 .. 1000) {
			$aqt_big->add($obj, $i / 10, $i / 10, 0.5);
		}
	},
	'small clear + insert 50' => sub {
		$aqt_small->clear;
		foreach my $i (1 .. 50) {
			$aqt_small->add($obj, $i / 4, $i / 4, 0.5);
		}
	},
	'get enclosed' => sub {
		$aqt_predeclared->getEnclosedObjects(50, 50, 10);
	},
};

