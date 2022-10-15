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

my $mock = MockObject->new;
my $mock_user = Model::User->new(-dummy);
$mock->add_method('save');
$mock->add_method('load');

DI->set('models', $mock->object, 1);
my $service = DI->get('user_service');

before_each {
	$mock->m('save')->clear;
};

$mock->m('save')->should_return(1);
test should_register_user => sub ($data) {
	my $user = $service->register_user($data);
	isa_ok $user, 'Model::User';
	ok $mock->m('save')->was_called, "mocked database save call $_";
	is $mock->m('save')->first_called_with, [exact_ref($user)], "mocked database save params $_";
};

before_each {
	$mock->m('load')->clear;
};

$mock->m('load')->should_return($mock_user);
test should_find_user_by_email => sub ($data) {
	my $user = $service->find_user_by_email($data);
	ok $mock->m('load')->was_called_once, "mocked database load call $_";
	is $user, exact_ref($mock_user), "mocked database load $_";
	is $mock->m('load')->called_with, [User => {email => $data}], "mocked database load params $_";
};

done_testing;

