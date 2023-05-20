requires 'maps/cape_peril';

my $max_level = 30;

define {
	max_level => $max_level,

	base_radius => 0.25,
	discover_radius => 5,
	yell_radius => 20,

	zero_stats => 8,
	starting_stats_floating => 5,
	stats_at_levels => [grep { $_ % 3 == 0 } 1 .. $max_level],

	base_speed => 0.8, # per second
	action_cooldown => 1, # how often can action be performed

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

	starting_location => location 'Cape Peril - Harbor',
	starting_location_x => 3,
	starting_location_y => 3,

	map_precision => 0.1, # how precise is the map movement?
};

