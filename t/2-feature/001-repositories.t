use Game::Ability;
use DI;
use Game::Model::User;
use Game::Character::Statistic;
use Test2::Tools::DatabaseTest;

use testheader;

database_test {
	### uses Repository::LoreData
	my $class = Game::Character::Statistic->get('STT_STR');
	is $class->lore_name, 'Siła', 'lore name ok';

	### test Repository::CharCache
	my $char_repo = DI->get('char_cache');
	ok $char_repo, 'character cache repo resolve ok';

	my $data = {
		level => 5,
		health_max => 300,
		health_regen => 1.5,
		mana_max => 150,
		mana_regen => 6,
		stats => 'STT_STR:30;STT_AGI:19;STT_INT:10;STT_STA:25',
	};

	my $model = Game::Model::Character->dummy->new();
	$char_repo->save($model->id, $data);

	my $loaded = $char_repo->load($model->id);
	is $loaded, $data, 'char cache repo save-load ok';

	$data->{health_max} += 5;
	ok $char_repo->save($model->id, $data), 'update ok';

	### test Repository::Schema
	my $user = Game::Model::User->dummy->new(
		email => 'brtastic.dev@gmail.com',
	);

	$user->set_password('test');
	my $schema_repo = DI->get('schema_repo');
	ok $schema_repo, 'schema repo resolve ok';

	ok !lives { $schema_repo->save($user) }, 'dummies cannot be saved';

	$user->promote;
	ok lives { $schema_repo->save($user) }, 'non-dummies can be saved';

	my $fetched = $schema_repo->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after save ok';

	$user->set_password('test2');
	ok lives { $schema_repo->save($user, 1) }, 'update ok';
	$fetched = $schema_repo->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after update ok';
};

done_testing;
