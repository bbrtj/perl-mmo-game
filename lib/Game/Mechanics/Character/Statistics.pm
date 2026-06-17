package Game::Mechanics::Character::Statistics;

use Exporter qw(import);
use Game::Config;
use Game::Helpers;

use header;

our @EXPORT_OK = qw(
	get_current_level
	get_exp_for_level
	get_size
	get_speed
	get_max_health
	get_health_regen
	get_max_energy
	get_energy_regen
	get_luck_effect
);

our %EXPORT_TAGS = (
	all => [@EXPORT_OK],
);

sub get_current_level ($exp)
{
	state $base_exp = Game::Config->base_exp;

	return 1 unless $exp >= $base_exp;
	my $level_approx = int((log($exp / ($base_exp / 2)) / log(2) / 2)**(100 / 58)) + 2;
	my $next_level_exp = get_exp_for_level($level_approx + 1);

	return $level_approx + ($exp >= $next_level_exp);
}

sub get_exp_for_level ($level)
{
	state $base_exp = Game::Config->base_exp;

	return ($level - 1) * $base_exp if $level < 3;
	my $result = int($base_exp / 2 * 2**(2 * ($level - 2)**(58 / 100)));
	my $magnitude = int(log($result) / log 10);
	my $precision = 10**int($magnitude / 2);

	return int($result / $precision) * $precision;
}

# size is affected by constitution
sub get_size ($race, $stats)
{
	state $base = Game::Config->base_size;
	state $stats0 = Game::Config->base_primary_stats;
	state $con_effect = lore_primary_stat('Constitution')->affects->{'size'};

	return $base * $race->size_multiplier
		* (1 + ($stats->{'pstat.con'} - $stats0) * $con_effect);
}

sub get_speed ($class, $stats)
{
	state $base = Game::Config->base_speed;
	state $stats0 = Game::Config->base_primary_stats;
	state $dex_effect = lore_primary_stat('Dexterity')->affects->{speed};

	return $base * $class->speed_multiplier
		* (1 + ($stats->{'pstat.dex'} - $stats0) * $dex_effect);
}

sub get_max_health ($class, $stats)
{
	state $base = Game::Config->base_health;
	state $stats0 = Game::Config->base_primary_stats;
	state $sta_effect = lore_secondary_stat('Stamina')->value;
	state $con_effect = lore_primary_stat('Constitution')->affects->{'sstat.stam'};

	return (
		$base * (
			1 + $stats->{'sstat.stam'} * $sta_effect
				* (1 + ($stats->{'pstat.con'} - $stats0) * $con_effect)
		)
	) * $class->health_multiplier;
}

sub get_health_regen ($class, $stats)
{
	state $base = Game::Config->base_health_regen;
	state $stats0 = Game::Config->base_primary_stats;
	state $vig_effect = lore_secondary_stat('Vigor')->value;
	state $wis_effect = lore_primary_stat('Wisdom')->affects->{'sstat.vigor'};

	return (
		$base * (
			1
				+ ($stats->{'sstat.vigor'} * $vig_effect)
				* (1 + ($stats->{'pstat.wis'} - $stats0) * $wis_effect)
		)
	) * $class->health_multiplier;
}

sub get_max_energy ($class, $stats)
{
	state $base = Game::Config->base_energy;
	state $stats0 = Game::Config->base_primary_stats;
	state $per_effect = lore_secondary_stat('Persistence')->value;
	state $cha_effect = lore_primary_stat('Charisma')->affects->{'sstat.pers'};

	return (
		$base * (
			1 +
				$stats->{'sstat.pers'} * $per_effect
				* (1 + ($stats->{'pstat.cha'} - $stats0) * $cha_effect)
		)
	) * $class->energy_multiplier;
}

sub get_energy_regen ($class, $stats)
{
	state $base = Game::Config->base_energy_regen;
	state $stats0 = Game::Config->base_primary_stats;
	state $vig_effect = lore_secondary_stat('Vigor')->value;
	state $wis_effect = lore_primary_stat('Wisdom')->affects->{'sstat.vigor'};

	return (
		$base * (
			1
				+ ($stats->{'sstat.vigor'} * $vig_effect)
				* (1 + ($stats->{'pstat.wis'} - $stats0) * $wis_effect)
		)
	) * $class->energy_multiplier;
}

sub get_luck_effect ($stats)
{
	state $stats0 = Game::Config->base_primary_stats;
	state $luck_effect = lore_primary_stat('Luck')->affects->{rolls};

	return 1 + ($stats->{'pstat.luck'} - $stats0) * $luck_effect;
}

