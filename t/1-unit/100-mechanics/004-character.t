use testheader;

use Test2::Tools::Compare qw(float);

use Game::Helpers;
use Game::Mechanics::Character::Statistics;

test_data 'should calculate experience for level' => [
	[1, 0],
	[2, 100],
	[3, 200],
	[4, 390],
	[30, 721000],
	[50, 24229000],
];

test_data 'should calculate health for character' => [
	[lore_class 'Warden', 0, 0, 80 * 1.6],
	[lore_class 'Warden', 1, 0, 80 * 1.6 * 1.025],
	[lore_class 'Warden', 1, 10, 80 * 1.6 * 1.05],
	[lore_class 'Warden', 1, 15, 80 * 1.6 * 1.0625],
	[lore_class 'Warden', 2, 15, 80 * 1.6 * 1.1250],
	[lore_class 'Assassin', 1, 10, 80 * 1.05],
	[lore_class 'Cultist', 5, 10, 80 * 0.9 * 1.25],
];

test_data 'should calculate health regeneration for character' => [
	[lore_class 'Warden', 0, 0, 0.4 * 1.6],
	[lore_class 'Warden', 1, 0, 0.4 * 1.6 * 1.08],
	[lore_class 'Warden', 1, 10, 0.4 * 1.6 * 1.1],
	[lore_class 'Warden', 1, 15, 0.4 * 1.6 * 1.11],
	[lore_class 'Warden', 2, 15, 0.4 * 1.6 * 1.22],
	[lore_class 'Assassin', 1, 10, 0.4 * 1.1],
	[lore_class 'Cultist', 5, 10, 0.4 * 0.9 * 1.5],
];

test_data 'should calculate energy for character' => [
	[lore_class 'Elementalist', 0, 0, 30 * 2],
	[lore_class 'Elementalist', 1, 0, 30 * 2 * 1.06],
	[lore_class 'Elementalist', 1, 10, 30 * 2 * 1.1],
	[lore_class 'Elementalist', 1, 15, 30 * 2 * 1.12],
	[lore_class 'Elementalist', 2, 15, 30 * 2 * 1.24],
	[lore_class 'Rogue', 1, 10, 30 * 1.1],
	[lore_class 'Knight', 5, 10, 30 * 1.5 * 1.5],
];

test_data 'should calculate energy regeneration for character' => [
	[lore_class 'Elementalist', 0, 0, 0.15 * 2],
	[lore_class 'Elementalist', 1, 0, 0.15 * 2 * 1.08],
	[lore_class 'Elementalist', 1, 10, 0.15 * 2 * 1.1],
	[lore_class 'Elementalist', 1, 15, 0.15 * 2 * 1.11],
	[lore_class 'Elementalist', 2, 15, 0.15 * 2 * 1.22],
	[lore_class 'Rogue', 1, 10, 0.15 * 1.1],
	[lore_class 'Knight', 5, 10, 0.15 * 1.5 * 1.5],
];

test_data 'should calculate size for character' => [
	[lore_race 'Animal', 10, 0.25],
	[lore_race 'Animal', 0, 0.25 * 0.8],
	[lore_race 'Dwarf', 15, 0.25 * 0.95 * 1.1],
	[lore_race 'Human', 8, 0.25 * 0.96],
];

test_data 'should calculate speed for character' => [
	[lore_class 'Rogue', 10, 0.8],
	[lore_class 'Rogue', 0, 0.8 * 0.8],
	[lore_class 'Rogue', 15, 0.8 * 1.1],
];

test should_calculate_experience_for_level => sub ($level, $experience) {
	is Game::Mechanics::Character::Statistics->get_exp_for_level($level), $experience, "experience for $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience), $level, "level $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience - 1), $level - 1, "previous level ok"
		if $level > 1;
};

test should_calculate_health_for_character => sub ($class_obj, $stamina, $constitution, $expected) {
	my $class = $class_obj->name;
	my %stats = (
		'sstat.stam' => $stamina,
		'pstat.con' => $constitution,
	);

	is Game::Mechanics::Character::Statistics->get_max_health($class_obj, \%stats),
		float($expected),
		"$class health ok ($stamina stamina, $constitution constitution)";
};

test should_calculate_health_regeneration_for_character => sub ($class_obj, $vigor, $wisdom, $expected) {
	my $class = $class_obj->name;
	my %stats = (
		'sstat.vigor' => $vigor,
		'pstat.wis' => $wisdom,
	);

	is Game::Mechanics::Character::Statistics->get_health_regen($class_obj, \%stats),
		float($expected),
		"$class health regen ok ($vigor vigor, $wisdom wisdom)";
};

test should_calculate_energy_for_character => sub ($class_obj, $persistence, $charisma, $expected) {
	my $class = $class_obj->name;
	my %stats = (
		'sstat.pers' => $persistence,
		'pstat.cha' => $charisma,
	);

	is Game::Mechanics::Character::Statistics->get_max_energy($class_obj, \%stats),
		float($expected),
		"$class energy ok ($persistence persistence, $charisma charisma)";
};

test should_calculate_energy_regeneration_for_character => sub ($class_obj, $vigor, $wisdom, $expected) {
	my $class = $class_obj->name;
	my %stats = (
		'sstat.vigor' => $vigor,
		'pstat.wis' => $wisdom,
	);

	is Game::Mechanics::Character::Statistics->get_energy_regen($class_obj, \%stats),
		float($expected),
		"$class energy regen ok ($vigor vigor, $wisdom wisdom)";
};

test should_calculate_size_for_character => sub ($race_obj, $constitution, $expected) {
	my $race = $race_obj->name;
	my %stats = (
		'pstat.con' => $constitution,
	);

	is Game::Mechanics::Character::Statistics->get_size($race_obj, \%stats),
		float($expected),
		"$race size ok ($constitution constitution)";
};

test should_calculate_speed_for_character => sub ($class_obj, $dexterity, $expected) {
	my $class = $class_obj->name;
	my %stats = (
		'pstat.dex' => $dexterity,
	);

	is Game::Mechanics::Character::Statistics->get_speed($class_obj, \%stats),
		float($expected),
		"$class speed ok ($dexterity dexterity)";
};

done_testing;

