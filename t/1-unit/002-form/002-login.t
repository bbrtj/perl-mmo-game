use Game::Form::Login;
use DI;
use Object::Sub;
use Model::User;
use Exception::RecordDoesNotExist;

use testheader;

BEGIN {
	test_data
		'login should succeed' => [
			[{email => 'test@test.com', password => 'abcdefg1', remember_me => 1}],
		],
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
		],
	;
}

my $tested_mail = 'test@test.com';
my $model = Model::User->dummy->new(email => $tested_mail);
$model->set_password('abcdefg1');
$model->promote;

DI->set('schema_repo', Object::Sub->new({
	load => sub ($self, $resultset, $params) {
		if ($resultset eq 'User') {
			Exception::RecordDoesNotExist->throw unless $params->{email} eq $tested_mail;
			return $model;
		}
		else {
			fail 'I did not expect any other resultset than User';
			Exception::RecordDoesNotExist->throw;
		}
	},
}), 1);

test_login_should_succeed sub ($data) {
	my $form = Game::Form::Login->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";

	if (!$form->valid) {
		diag Dumper($form->errors_hash);
	}
	else {
		is $form->user, exact_ref($model), 'fetched model ok';
	}
};

test_login_should_fail sub ($data, $errors) {
	my $form = Game::Form::Login->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";
};

done_testing;

