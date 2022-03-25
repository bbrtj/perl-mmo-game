my $max_level = 30;

define {
	max_level => $max_level,

	zero_stats => 8,
	starting_stats_floating => 5,
	stats_at_levels => [grep { $_ % 3 == 0 } 1 .. $max_level],

	critical_damage => 1.5,

	# these will be modified by primary stats multipliers
	damage_per_might => 1, # %
	damage_per_acumen => 1, # %
	crit_per_accuracy => 0.5, # %
	health_per_stamina => 4,
	energy_per_persistence => 5,
	reduction_per_resilience => 0.08, # %
	reduction_per_willpower => 0.08, # %
	regeneration_per_vigor => 0.1, # %
};

