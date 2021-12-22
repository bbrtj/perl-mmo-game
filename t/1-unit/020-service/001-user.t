use DI;
use Model::User;

use testheader;

BEGIN {
	test_data
		'should register user' => [
			[{email => 'test@test.com', password => 'abcdefg1'}],
		],
		'should find user by email' => [
			['test@test.com'],
		],
	;
}

my $mock = MockObject->new;
my $mock_user = Model::User->dummy->new;
my $save_mock = $mock->add_method('save');
my $load_mock = $mock->add_method('load');

DI->set('schema_repo', $mock->object, 1);
my $service = DI->get('user_service');

$save_mock->should_return(1);
test_should_register_user sub ($data) {
	$save_mock->clear;

	my $user = $service->register_user($data);
	isa_ok $user, 'Model::User';
	ok $save_mock->was_called, "mocked database save call $_";
	is $save_mock->first_called_with, [exact_ref($user)], "mocked database save params $_"
};

$load_mock->should_return($mock_user);
test_should_find_user_by_email sub ($data) {
	$load_mock->clear;

	my $user = $service->find_user_by_email($data);
	ok $load_mock->was_called_once, "mocked database load call $_";
	is $user, exact_ref($mock_user), "mocked database load $_";
	is $load_mock->called_with, [User => {email => $data}], "mocked database load params $_"
};

done_testing;
