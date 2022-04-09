use DI;
use Model::User;
use Utils;

use testheader;

test_data
	'should register user' => [
		[{email => 'test@test.com', password => 'abcdefg1'}],
	],
	'should find user by email' => [
		['test@test.com'],
	];

Utils->bootstrap_lore;

my $mock = MockObject->new;
my $mock_user = Model::User->new;
my $save_mock = $mock->add_method('save');
my $load_mock = $mock->add_method('load');

DI->set('models', $mock->object, 1);
my $service = DI->get('user_service');

before_each {
	$save_mock->clear;
};

$save_mock->should_return(1);
test should_register_user => sub ($data) {
	my $user = $service->register_user($data);
	isa_ok $user, 'Model::User';
	ok $save_mock->was_called, "mocked database save call $_";
	is $save_mock->first_called_with, [exact_ref($user)], "mocked database save params $_";
};

before_each {
	$load_mock->clear;
};

$load_mock->should_return($mock_user);
test should_find_user_by_email => sub ($data) {
	my $user = $service->find_user_by_email($data);
	ok $load_mock->was_called_once, "mocked database load call $_";
	is $user, exact_ref($mock_user), "mocked database load $_";
	is $load_mock->called_with, [User => {email => $data}], "mocked database load params $_";
};

done_testing;

