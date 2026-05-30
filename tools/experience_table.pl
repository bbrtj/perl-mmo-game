use v5.42;
use Game::Mechanics::Character::Statistics;

my $last = 0;
for my $level (1 .. 60) {
	my $exp = Game::Mechanics::Character::Statistics->get_exp_for_level($level);
	my $perc = $last ? int($exp / $last * 10) / 10 : 'N/A';
	$last = $exp;

	say "level $level: $exp, increase: $perc";
}

