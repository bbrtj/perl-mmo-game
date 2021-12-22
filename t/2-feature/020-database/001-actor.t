use DI;
use ActorTest;
use Test2::Tools::DatabaseTest;

use testheader;

database_test {
	my ($actor, @related_models) = ActorTest->create_actor;
	foreach my $model (@related_models) {
		DI->get('schema_repo')->save($model);
	}

	DI->get('actor_unit')->save($actor);
	my $loaded = DI->get('actor_unit')->load($actor->character->id);
	is $loaded, $actor, 'repository stored actor data ok';
};

done_testing;
