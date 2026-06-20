BEGIN { $ENV{ALGORITHM_QUADTREE_BACKEND} = 'Algorithm::QuadTree::XS::NoBackRefs'; }
use Algorithm::QuadTree;

use header;

use Benchmark::Dumb qw(timethese);

# depth = 11 will divide each dimension by 2^10 (1024), so this quadtree will be
# accurate to 50 / 1024 = at least 0.05 unit
my $aqt_combat = Algorithm::QuadTree->new(
	-depth => 11,
	-xmin => 0,
	-ymin => 0,
	-xmax => 50,
	-ymax => 50,
);

my $aqt_discovery = Algorithm::QuadTree->new(
	-depth => 7,
	-xmin => 0,
	-ymin => 0,
	-xmax => 50,
	-ymax => 50,
);

sub insert_objects ($aqt)
{
	foreach my $x (1 .. 25) {
		foreach my $y (1 .. 25) {
			$aqt->add("obj$x$y", $x * 2, $y * 2, 0.25);
		}
	}
}

timethese 200.01, {
	'combat refresh' => sub {
		$aqt_combat->clear;
		insert_objects($aqt_combat);
	},
	'discovery refresh' => sub {
		$aqt_discovery->clear;
		insert_objects($aqt_discovery);
	},
	'get enclosed (small aoe)' => sub {
		$aqt_combat->getEnclosedObjects(50, 50, 0.5);
	},
	'get enclosed (big aoe)' => sub {
		$aqt_combat->getEnclosedObjects(50, 50, 2);
	},
	'get enclosed (discovery)' => sub {
		$aqt_discovery->getEnclosedObjects(50, 50, 6);
	},
};

