use Web::Form::Login;
use DI;
use Model::User;
use Exception::RecordDoesNotExist;
use Utils;

use testheader;

test_data
	'login should succeed' => [
		[{email => 'test@test.com', password => 'abcdefg1', remember_me => 1}],
	];

test_data
	'login should fail' => [
		[
			{email => 'test@test.com', password => 'abcdefgh'},
			{'' => ['Invalid email or password']},
		],
		[
			{email => 'test@test.com', password => 'Abcdefg1'},
			{'' => ['Invalid email or password']},
		],
		[
			{email => 'testa@test.com', password => 'abcdefgh'},
			{'' => ['Invalid email or password']},
		],
		[
			{email => '', password => 'abcdefgh'},
			{email => ['Field is required']},
		],
	];

Utils->bootstrap_lore;
my $tested_mail = 'test@test.com';
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

test login_should_succeed => sub ($data) {
	my $form = Web::Form::Login->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";

	if (!$form->valid) {
		diag Dumper($form->errors_hash);
	}
	else {
		is $form->user, exact_ref($mock_model), 'fetched model ok';

		ok $load_mock->was_called_once, "mock called once $_";
		is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
	}
};

test login_should_fail => sub ($data, $errors) {
	my $form = Web::Form::Login->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";

	# mock might not get called because db is queried in form cleaner
	if ($load_mock->was_called) {
		is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
	}
};

done_testing;

