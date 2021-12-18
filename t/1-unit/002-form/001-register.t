use Game::Form::Register;

use testheader;

BEGIN {
	test_data
		'registration should succeed' => [
			[{email => 'test@test.com', password => 'abcdefg1', repeat_password => 'abcdefg1'}],
			[{email => 'test@test.com', password => 'abcdefgH1-', repeat_password => 'abcdefgH1-'}],
		],
		'registration should fail' => [
			[
				{email => 'test@test.com', password => 'abcdefgh1', repeat_password => 'abcdefa'},
				{'' => ['passwords do not match']}
			],
			[
				{email => 'test@test.com', password => 'abcdefa', repeat_password => 'abcdefa'},
				{'password' => ['password must have at least 8 characters', 'password must contain a digit']}
			],
			[
				{email => 'test@test.com', password => 'abcdef1', repeat_password => 'abcdef1'},
				{'password' => ['password must have at least 8 characters']}
			],
			[
				{email => 'test@test.com', password => 'abcdefgh', repeat_password => 'abcdefgh'},
				{'password' => ['password must contain a digit']}
			],
			[
				{email => '', password => 'abcdefg1', repeat_password => 'abcdefg1'},
				{'email' => ['field is required']}
			],
			[
				{email => 'test@test.com', password => '', repeat_password => 'abcdefg1'},
				{'password' => ['field is required']}
			],
			[
				{email => 'test@test.com', password => 'abcdefg1', repeat_password => ''},
				{'repeat_password' => ['field is required']}
			],
		],
		;
}

test_registration_should_succeed sub ($data) {
	my $form = Game::Form::Register->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";
};

test_registration_should_fail sub ($data, $errors) {
	my $form = Game::Form::Register->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";
};

done_testing;

