use Model;
use Utils;
use Game::Helpers;

use testheader;

Utils->bootstrap_lore;

test_data
	'should create player' => [
		[{name => 'testPlayer', class => lore_class('Trapper')}],
		[{name => 'testPlayer2', class => lore_class('Assassin')}],
	];

my $dbmock = MockObject->new;
$dbmock->add_method('transaction')->should_call(
	sub ($self, $code) {
		return $code->();
	}
);

my $mock = MockObject->new;
$mock->add_method('save', 1);
$mock->add_method('db', $dbmock->object);

DI->set('models', $mock->object, 1);
my $service = DI->get('character_service');

before_each {
	$mock->m('save')->clear;
	$dbmock->m('transaction')->clear;
};

test should_create_player => sub ($data) {
	my $user = Model::User->new;
	my $player = $service->create_player($user, $data);

	isa_ok $player, 'Model::Player';
	is $player->user_id, $user->id, "player user id set in $_";

	ok $dbmock->m('transaction')->was_called_once, "mocked database transaction single call $_";

	# 3 save calls - player, character and variables
	is $mock->m('save')->called_times, 3, "mocked database save call $_";
	is $mock->m('save')->first_called_with, [exact_ref($player)], "mocked database save params $_";
};

done_testing;

