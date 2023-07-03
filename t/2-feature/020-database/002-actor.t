use ActorTest;
use Test2::Tools::DatabaseTest;
use Utils;

use testheader;

database_test {
	my ($actor, %related_models) = ActorTest->save_actor;
	my $units = DI->get('units_repo');

	subtest 'stores character variables', sub {
		$related_models{variables}->set_experience(1500);
		$units->save($actor);
		my $loaded = $units->load_actor($actor->character->id);

		ok eq_deeply($loaded, $actor), 'repository stored actor data ok';
	};

	subtest 'does not store character data', sub {
		$related_models{character}->set_name('Priesty');
		$units->save($actor);
		my $loaded = $units->load_actor($actor->character->id);

		ok !eq_deeply($loaded, $actor), 'repository did not store actor data ok';
	};
};

done_testing;

