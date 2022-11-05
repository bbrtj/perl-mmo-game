use all 'Model';
use Test2::Tools::DatabaseTest;
use Utils;

use testheader;

database_test {
	### test Repository::Models
	my $user = Model::User->new(
		plaintext_password => 'test',
		email => 'brtastic.dev@gmail.com',
	);

	my $models = DI->get('models_repo');
	ok $models, 'schema repo resolve ok';

	$models->save($user);

	my $fetched = $models->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after save ok';

	$user->set_password('test2');
	$models->save($user, 1);
	$fetched = $models->load(User => $user->id);
	is $fetched->serialize, $user->serialize, 'after update ok';
};

done_testing;

