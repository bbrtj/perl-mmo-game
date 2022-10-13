use Model;
use Utils;
use Game::Helpers;

use testheader;

Utils->bootstrap_lore;

test_data
	'should create player' => [
		[{name => 'testPlayer', class => lore_class('Trapper')}],
	];

my $mock = MockObject->new;
my $save_mock = $mock->add_method('save', 1);

my $dbmock = MockObject->new;
my $txn_mock = $dbmock->add_method('transaction');
$txn_mock->should_call(sub ($self, $code) { $code->() });
$mock->add_method('db', $dbmock->object);

DI->set('models', $mock->object, 1);
my $service = DI->get('character_service');

before_each {
	$save_mock->clear;
};

test should_create_player => sub ($data) {
	my $user = Model::User->new;
	my $player = $service->create_player($user, $data);

	isa_ok $player, 'Model::Player';
	is $player->user_id, $user->id, "player user id set in $_";

	ok $txn_mock->was_called_once, "mocked database transaction single call $_";

	# 3 save calls - player, character and variables
	is $save_mock->called_times, 3, "mocked database save call $_";
	is $save_mock->first_called_with, [exact_ref($player)], "mocked database save params $_";
};

done_testing;

