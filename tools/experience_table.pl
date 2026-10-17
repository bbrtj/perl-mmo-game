use v5.42;
use Game::Mechanics::Character::Statistics qw(get_exp_for_level get_monster_exp);

my $last = 0;
for my $level (2 .. 60) {
	my $exp = get_exp_for_level($level);
	my $monster_exp = get_monster_exp($level - 1);
	my $perc = $last ? int($exp / $last * 10) / 10 : 'N/A';
	my $kills = int(($exp - $last // 0) / $monster_exp + 0.5);
	$last = $exp;

	say "level $level: $exp, increase: $perc, monsters: $kills";
}

