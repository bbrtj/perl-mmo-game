use Web::Form::Login;
use Form::Login;
use Model::User;
use X::RecordDoesNotExist;
use Utils;

use testheader;

test_data
	'web login should succeed' => [
		[{email => 'test@test.com', password => 'abcdefg1', remember_me => 1}],
	];

test_data
	'login should succeed' => [
		[{email => 'test@test.com', password => 'abcdefg1'}],
	];

test_data
	'web login should fail' => [
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

test_data
	'login should fail' => [
		[
			{email => 'test@test.com', password => 'abcdefgh'},
			{'' => ['err.invalid_credentials']},
		],
		[
			{email => 'test@test.com', password => 'Abcdefg1'},
			{'' => ['err.invalid_credentials']},
		],
		[
			{email => 'testa@test.com', password => 'abcdefgh'},
			{'' => ['err.invalid_credentials']},
		],
		[
			{email => '', password => 'abcdefgh'},
			{email => ['field is required']},
		],
	];

Utils->bootstrap_lore;
my $tested_mail = 'test@test.com';
my $mock_model = Model::User->new(email => $tested_mail);
$mock_model->set_password('abcdefg1');
$mock_model->check;

my $mock = MockObject->new;
my $load_mock = $mock->add_method('load')
	->should_call(
		sub ($self, $resultset, $params) {
			X::RecordDoesNotExist->throw unless $params->{email} eq $tested_mail;
			return $mock_model;
		}
	);

DI->set('models', $mock->object, 1);

before_each {
	$load_mock->clear;
};

for my $prefix ('', 'web') {
	my $class = join '::', grep { $_ } (ucfirst $prefix, "Form::Login");
	my $test_prefix = $prefix ? $prefix . '_' : '';

	test $test_prefix . login_should_succeed => sub ($data) {
		$_ .= " ($class)";

		my $form = $class->new;
		$form->set_input($data);

		my $valid = $form->valid;
		ok $valid, "form valid $_";

		if (!$valid) {
			diag Dumper($form->errors_hash);
		}
		else {
			is $form->user, exact_ref($mock_model), 'fetched model ok';

			ok $load_mock->was_called_once, "mock called once $_";
			is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
		}
	};

	test $test_prefix . login_should_fail => sub ($data, $errors) {
		$_ .= " ($class)";

		my $form = $class->new;
		$form->set_input($data);
		ok !$form->valid, "form invalid $_";
		is $form->errors_hash, $errors, "errors hash $_";

		# mock might not get called because db is queried in form cleaner
		if ($load_mock->was_called) {
			is $load_mock->called_with, [User => {email => $data->{email}}], "mock called parameters $_";
		}
	};
}

done_testing;

