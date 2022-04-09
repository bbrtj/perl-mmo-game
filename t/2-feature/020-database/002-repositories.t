use DI;
use Model::User;
use Test2::Tools::DatabaseTest;
use Game::Helpers;
use Utils;

use testheader;

Utils->bootstrap_lore;

database_test {
	### uses Repository::LoreData
	local $i18n::CURRENT_LANG = 'pl';

	my $class = lore_primary_stat 'Strength';
	is $class->data->translations->{pl}{name}, 'Siła', 'lore name ok';

	### test Repository::CharCache
	# my $char_repo = DI->get('char_cache');
	# ok $char_repo, 'character cache repo resolve ok';

	# my $data = {
	# 	level => 5,
	# 	health_max => 300,
	# 	health_regen => 1.5,
	# 	mana_max => 150,
	# 	mana_regen => 6,
	# 	stats => 'STT_STR:30;STT_AGI:19;STT_INT:10;STT_STA:25',
	# };

	# my $model = Model::Character->new();
	# $char_repo->save($model->id, $data);

	# my $loaded = $char_repo->load($model->id);
	# is $loaded, $data, 'char cache repo save-load ok';

	# $data->{health_max} += 5;
	# ok $char_repo->save($model->id, $data), 'update ok';

	### test Repository::Schema
	my $user = Model::User->new(
		email => 'brtastic.dev@gmail.com',
	);

	$user->set_password('test');
	my $models = DI->get('models');
	ok $models, 'schema repo resolve ok';

	$user->promote;
	ok lives { $models->save($user) }, 'Models can be saved';

	my $fetched = $models->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after save ok';

	$user->set_password('test2');
	ok lives { $models->save($user, 1) }, 'update ok';
	$fetched = $models->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after update ok';
};

done_testing;

