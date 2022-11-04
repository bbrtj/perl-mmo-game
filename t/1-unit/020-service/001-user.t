use Model;
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

my $mock = Test::Spy->new(imitates => 'Repository::Models');
my $mock_user = Model::User->dummy;
$mock->add_method('save', 1);
$mock->add_method('load', $mock_user);

DI->set('models', $mock->object, 1);
my $service = DI->get('user_service');

before_each {
	$mock->set_context('save');
	$mock->clear;
};

test should_register_user => sub ($data) {
	my $user = $service->register_user($data);
	isa_ok $user, 'Model::User';
	ok $mock->was_called, "mocked database save call $_";
	is $mock->first_called_with, [exact_ref($user)], "mocked database save params $_";
};

before_each {
	$mock->set_context('load');
	$mock->clear;
};

test should_find_user_by_email => sub ($data) {
	my $user = $service->find_user_by_email($data);
	ok $mock->was_called_once, "mocked database load call $_";
	is $user, exact_ref($mock_user), "mocked database load $_";
	is $mock->called_with, [User => {email => $data}], "mocked database load params $_";
};

done_testing;

