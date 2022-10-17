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

my $dbmock = Test::Spy->new(context => 'transaction');
$dbmock->add_method('transaction')->should_call(
	sub ($self, $code) {
		return $code->();
	}
);

my $mock = Test::Spy->new;
$mock->add_method('save', 1);
$mock->add_method('db', $dbmock->object);

DI->set('models', $mock->object, 1);
my $service = DI->get('character_service');

before_each {
	$mock->method('save')->clear;
	$dbmock->clear;
};

test should_create_player => sub ($data) {
	my $user = Model::User->dummy;
	my $player = $service->create_player($user, $data);

	isa_ok $player, 'Model::Player';
	is $player->user_id, $user->id, "player user id set in $_";

	ok $dbmock->was_called_once, "mocked database transaction single call $_";

	# 3 save calls - player, character and variables
	$mock->set_context('save');
	is $mock->called_times, 3, "mocked database save call $_";
	is $mock->first_called_with, [exact_ref($player)], "mocked database Player save params $_";

	is $mock->next_called_with, [check_isa('Model::Character')], "mocked database Character save params $_";
	is $mock->called_with->[0]->name, ucfirst lc $data->{name}, "mocked database character name $_";

	is $mock->next_called_with, [check_isa('Model::CharacterVariables')], "mocked database CharacterVariables save params $_";
	is $mock->called_with->[0]->pos_x, Game::Config->config->{starting_location_x}, "mocked database pos_x $_";
	is $mock->called_with->[0]->pos_y, Game::Config->config->{starting_location_y}, "mocked database pos_y $_";
};

done_testing;

