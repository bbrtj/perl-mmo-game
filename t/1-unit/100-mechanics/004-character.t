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
	['Warden', 0, 0, 80 * 1.6],
	['Warden', 1, 0, 80 * 1.6 * 1.025],
	['Warden', 1, 10, 80 * 1.6 * 1.05],
	['Warden', 1, 15, 80 * 1.6 * 1.0625],
	['Warden', 2, 15, 80 * 1.6 * 1.1250],
	['Assassin', 1, 10, 80 * 1.05],
	['Cultist', 5, 10, 80 * 0.9 * 1.25],
];

test_data 'should calculate health regeneration for character' => [
	['Warden', 0, 0, 0.4 * 1.6],
	['Warden', 1, 0, 0.4 * 1.6 * 1.08],
	['Warden', 1, 10, 0.4 * 1.6 * 1.1],
	['Warden', 1, 15, 0.4 * 1.6 * 1.11],
	['Warden', 2, 15, 0.4 * 1.6 * 1.22],
	['Assassin', 1, 10, 0.4 * 1.1],
	['Cultist', 5, 10, 0.4 * 0.9 * 1.5],
];

test_data 'should calculate energy for character' => [
	['Elementalist', 0, 0, 30 * 2],
	['Elementalist', 1, 0, 30 * 2 * 1.06],
	['Elementalist', 1, 10, 30 * 2 * 1.1],
	['Elementalist', 1, 15, 30 * 2 * 1.12],
	['Elementalist', 2, 15, 30 * 2 * 1.24],
	['Rogue', 1, 10, 30 * 1.1],
	['Knight', 5, 10, 30 * 1.5 * 1.5],
];

test_data 'should calculate energy regeneration for character' => [
	['Elementalist', 0, 0, 0.15 * 2],
	['Elementalist', 1, 0, 0.15 * 2 * 1.08],
	['Elementalist', 1, 10, 0.15 * 2 * 1.1],
	['Elementalist', 1, 15, 0.15 * 2 * 1.11],
	['Elementalist', 2, 15, 0.15 * 2 * 1.22],
	['Rogue', 1, 10, 0.15 * 1.1],
	['Knight', 5, 10, 0.15 * 1.5 * 1.5],
];

test should_calculate_experience_for_level => sub ($level, $experience) {
	is Game::Mechanics::Character::Statistics->get_exp_for_level($level), $experience, "experience for $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience), $level, "level $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience - 1), $level - 1, "previous level ok"
		if $level > 1;
};

test should_calculate_health_for_character => sub ($class, $stamina, $constitution, $expected) {
	my $class_obj = lore_class $class;
	my %stats = (
		'sstat.stam' => $stamina,
		'pstat.con' => $constitution,
	);

	is Game::Mechanics::Character::Statistics->get_max_health($class_obj, \%stats),
		float($expected),
		"$class health ok ($stamina stamina, $constitution constitution)";
};

test should_calculate_health_regeneration_for_character => sub ($class, $vigor, $wisdom, $expected) {
	my $class_obj = lore_class $class;
	my %stats = (
		'sstat.vigor' => $vigor,
		'pstat.wis' => $wisdom,
	);

	is Game::Mechanics::Character::Statistics->get_health_regen($class_obj, \%stats),
		float($expected),
		"$class health regen ok ($vigor vigor, $wisdom wisdom)";
};

test should_calculate_energy_for_character => sub ($class, $persistence, $charisma, $expected) {
	my $class_obj = lore_class $class;
	my %stats = (
		'sstat.pers' => $persistence,
		'pstat.cha' => $charisma,
	);

	is Game::Mechanics::Character::Statistics->get_max_energy($class_obj, \%stats),
		float($expected),
		"$class energy ok ($persistence persistence, $charisma charisma)";
};

test should_calculate_energy_regeneration_for_character => sub ($class, $vigor, $wisdom, $expected) {
	my $class_obj = lore_class $class;
	my %stats = (
		'sstat.vigor' => $vigor,
		'pstat.wis' => $wisdom,
	);

	is Game::Mechanics::Character::Statistics->get_energy_regen($class_obj, \%stats),
		float($expected),
		"$class energy regen ok ($vigor vigor, $wisdom wisdom)";
};

done_testing;

