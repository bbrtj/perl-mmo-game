use Algorithm::QuadTree;

use header;

use Benchmark::Dumb qw(timethese);

# depth = 8 will divide each dimension by 2^7 (128), so this quadtree will be
# accurate to 100 / 128 = at least 1 unit
my $aqt_combat = Algorithm::QuadTree->new(
	-depth => 8,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

my $aqt_discovery = Algorithm::QuadTree->new(
	-depth => 8,
	-xmin => 0,
	-ymin => 0,
	-xmax => 100,
	-ymax => 100,
);

# NOTE: worst case scenario - the zone is full of actors
sub insert_objects ($aqt, $size = 0.25)
{
	foreach my $x (1 .. 33) {
		foreach my $y (1 .. 33) {
			$aqt->add("obj$x$y", $x * 3, $y * 3, $size);
		}
	}
}

# makes sure enclosed benchmarks will have something to work with if they were
# to run first
insert_objects($aqt_combat);
insert_objects($aqt_discovery);

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
		$aqt_combat->get(25, 25, 0.5);
	},
	'get enclosed (big aoe)' => sub {
		$aqt_combat->get(25, 25, 2);
	},
	'get enclosed (discovery)' => sub {
		$aqt_discovery->getApprox(25, 25, 6);
	},
};

