use DI;
use ActorTest;
use Test2::Tools::DatabaseTest;

use testheader;

database_test {
	my ($actor, %related_models) = ActorTest->create_actor;
	foreach my $model (@related_models{qw(user player character variables)}) {
		DI->get('schema_repo')->save($model);
	}

	$related_models{variables}->set_experience(1500);
	$related_models{character}->set_name('Priesty');

	DI->get('units')->save($actor);
	my $loaded = DI->get('units')->get_actor($actor->character->id);
	is $loaded, $actor, 'repository stored actor data ok';
};

done_testing;
