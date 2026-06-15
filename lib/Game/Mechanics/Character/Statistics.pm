package Game::Mechanics::Character::Statistics;

use Game::Config;

use header;

use constant FIRST_LEVEL => 100;

sub get_current_level ($self, $exp)
{
	return 1 unless $exp >= FIRST_LEVEL;
	my $level_approx = int((log($exp / (FIRST_LEVEL / 2)) / log(2) / 2)**(100 / 58)) + 2;
	my $next_level_exp = $self->get_exp_for_level($level_approx + 1);

	return $level_approx + ($exp >= $next_level_exp);
}

sub get_exp_for_level ($self, $level)
{
	return ($level - 1) * FIRST_LEVEL if $level < 3;
	my $result = int(FIRST_LEVEL / 2 * 2**(2 * ($level - 2)**(58 / 100)));
	my $magnitude = int(log($result) / log 10);
	my $precision = 10**int($magnitude / 2);

	return int($result / $precision) * $precision;
}

# size is affected by constitution
sub get_size ($self, $race, $class, $primary_stats)
{
	state $base = Game::Config->base_size;
	state $stats0 = Game::Config->base_stats;

	return $base * $race->size_multiplier * $class->size_multiplier
		* (1 + ($primary_stats->{'pstat.con'} - $stats0) / 50);
}

# TODO: health bonuses from abilities
sub get_max_health ($self, $level, $class, $primary_stats)
{
	state $base = Game::Config->base_health;
	state $stats0 = Game::Config->base_stats;

	return ($base + ($level - 1) * $base * 0.2)
		* $class->health_multiplier
		* (1 + ($primary_stats->{'pstat.con'} - $stats0) / 20);
}

# TODO: energy bonuses from abilities
sub get_max_energy ($self, $level, $class, $primary_stats)
{
	state $base = Game::Config->base_energy;
	state $stats0 = Game::Config->base_stats;

	return ($base + ($level - 1) * $base * 0.25)
		* $class->energy_multiplier
		* (1 + ($primary_stats->{'pstat.wis'} - $stats0) / 15);
}

sub get_speed ($self, $primary_stats)
{
	state $base = Game::Config->base_speed;
	state $stats0 = Game::Config->base_stats;

	return $base
		* (1 + ($primary_stats->{'pstat.dex'} - $stats0) / 20);
}

