use Web::Form::Register;
use DI;
use Model::User;
use Exception::RecordDoesNotExist;

use testheader;

test_data
	'registration should succeed' => [
		[{email => 'test@test.com', password => 'abcdefg1', repeat_password => 'abcdefg1'}],
		[{email => 'test@test.com', password => 'abcdefgH1-', repeat_password => 'abcdefgH1-'}],
	];

test_data
	'registration should fail' => [
		[
			{email => 'test@test.com', password => 'abcdefgh1', repeat_password => 'abcdefa'},
			{'' => ['Passwords do not match']}
		],
		[
			{email => 'test@test.com', password => 'abcdefa', repeat_password => 'abcdefa'},
			{'password' => ['Password must have at least 8 characters', 'Password must contain a digit']}
		],
		[
			{email => 'test@test.com', password => 'abcdef1', repeat_password => 'abcdef1'},
			{'password' => ['Password must have at least 8 characters']}
		],
		[
			{email => 'test@test.com', password => 'abcdefgh', repeat_password => 'abcdefgh'},
			{'password' => ['Password must contain a digit']}
		],
		[
			{email => '', password => 'abcdefg1', repeat_password => 'abcdefg1'},
			{'email' => ['Field is required']}
		],
		[
			{email => 'test@test.com', password => '', repeat_password => 'abcdefg1'},
			{'password' => ['Field is required']}
		],
		[
			{email => 'test@test.com', password => 'abcdefg1', repeat_password => ''},
			{'repeat_password' => ['Field is required']}
		],
		[
			{email => 'test2@test.com', password => 'abcdefg1', repeat_password => 'abcdefg1'},
			{'' => ['That email is already taken']}
		],
	];

my $tested_mail = 'test2@test.com';
my $mock_model = Model::User->new(email => $tested_mail);
$mock_model->set_password('abcdefg1');
$mock_model->promote;

my $mock = MockObject->new;
my $load_mock = $mock->add_method('load')
	->should_call(
		sub ($resultset, $params) {
			Exception::RecordDoesNotExist->throw unless $params->{email} eq $tested_mail;
			return $mock_model;
		}
	);

DI->set('schema_repo', $mock->object, 1);

before_each {
	$load_mock->clear;
};

test registration_should_succeed => sub ($data) {
	my $form = Web::Form::Register->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";

	ok $load_mock->was_called_once, "mock called once $_";
	is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
};

test registration_should_fail => sub ($data, $errors) {
	my $form = Web::Form::Register->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";

	# mock might not get called because db is queried in form cleaner
	if ($load_mock->was_called) {
		is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
	}
};

done_testing;

