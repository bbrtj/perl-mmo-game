use testheader;

use Game::Mechanics::Character::Statistics;

test_data
	'should calculate experience for level' => [
		[1, 0],
		[2, 100],
		[3, 200],
		[4, 390],
		[30, 721000],
		[50, 24229000],
	];

test should_calculate_experience_for_level => sub ($level, $experience) {
	is Game::Mechanics::Character::Statistics->get_exp_for_level($level), $experience, "experience for $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience), $level, "level $level ok";
	is Game::Mechanics::Character::Statistics->get_current_level($experience - 1), $level - 1, "previous level ok"
		if $level > 1;
};

done_testing;

