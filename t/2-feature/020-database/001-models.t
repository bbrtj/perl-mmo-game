use all 'Model';
use Test2::Tools::DatabaseTest;
use Utils;

use testheader;

database_test {
	### test Repository::Models
	my $user = Model::User->dummy(
		email => 'brtastic.dev@gmail.com',
	);

	$user->set_password('test');
	my $models = DI->get('models_repo');
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

