use Algorithm::QuadTree;

use header;

use Benchmark::Dumb qw(cmpthese);

my $aqt_predeclared = Algorithm::QuadTree->new(
	-depth => 9,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

# depth = 9 will divide each dimension by 2^9 (512), so this quadtree will be
# accurate to 100 / 512 = at least 0.2 unit
my $aqt_big = Algorithm::QuadTree->new(
	-depth => 9,
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
	$aqt_predeclared->add($obj, rand 100, rand 100, 0.25);
}

cmpthese 200.01, {
	'big clear + insert 1000' => sub {
		$aqt_big->clear;
		foreach my $i (1 .. 1000) {
			$aqt_big->add($obj, $i / 10, $i / 10, 0.25);
		}
	},
	'small clear + insert 50' => sub {
		$aqt_small->clear;
		foreach my $i (1 .. 50) {
			$aqt_small->add($obj, $i / 4, $i / 4, 0.25);
		}
	},
	'get enclosed' => sub {
		$aqt_predeclared->getEnclosedObjects(50, 50, 10);
	},
};

