use Algorithm::QuadTree;

use header;

use Benchmark::Dumb qw(cmpthese);

my $aqt_predeclared = Algorithm::QuadTree->new(
	-depth => 11,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

# depth = 10 will divide each dimension by 2^10 (1024), so this quadtree will be
# accurate to 100 / 1024 = at least 0.1 unit
my $aqt_big = Algorithm::QuadTree->new(
	-depth => 11,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

my $aqt_small = Algorithm::QuadTree->new(
	-depth => 9,
	-xmin => 0,
	-ymin => 0,
	-xmax => 15,
	-ymax => 25,
);

my $obj = 'obj';

foreach my $i (1 .. 1000) {
	$aqt_predeclared->add($obj, $i / 10, $i / 10, 0.25);
}

cmpthese 200.01, {
	'big clear + insert 1000' => sub {
		$aqt_big->clear;
		foreach my $i (1 .. 1000) {
			$aqt_big->add($obj, $i / 10, $i / 10, 0.25);
		}
	},
	'small clear + insert 100' => sub {
		$aqt_small->clear;
		foreach my $i (1 .. 100) {
			$aqt_small->add($obj, $i / 4, $i / 4, 0.25);
		}
	},
	'get enclosed (small aoe)' => sub {
		$aqt_predeclared->getEnclosedObjects(50, 50, 0.5);
	},
	'get enclosed (big aoe)' => sub {
		$aqt_predeclared->getEnclosedObjects(50, 50, 2);
	},
	'get enclosed (discovery)' => sub {
		$aqt_predeclared->getEnclosedObjects(50, 50, 6);
	},
};

