use DI;
use ActorTest;
use Test2::Tools::DatabaseTest;
use Utils;

use testheader;

Utils->bootstrap_lore;

database_test {
	my ($actor, %related_models) = ActorTest->create_actor;
	foreach my $model (@related_models{qw(user player character)}) {
		DI->get('models')->save($model);
	}

	# insert, not update
	DI->get('units')->save($actor, 0);

	subtest 'stores character variables', sub {
		$related_models{variables}->set_experience(1500);
		DI->get('units')->save($actor);
		my $loaded = DI->get('units')->load_actor($actor->character->id);
		is $loaded, $actor, 'repository stored actor data ok';
	};

	subtest 'does not store character data', sub {
		$related_models{character}->set_name('Priesty');
		DI->get('units')->save($actor);
		my $loaded = DI->get('units')->load_actor($actor->character->id);
		isnt $loaded, $actor, 'repository did not store actor data ok';
	};
};

done_testing;

