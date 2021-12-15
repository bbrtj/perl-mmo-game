use Test::More;
use Test::Exception;
use DI;
use DatabaseTest;
use ActorTest;

use header -noclean;

DatabaseTest->test(
	sub {
		my ($actor, @related_models) = ActorTest->create_actor;
		foreach my $model (@related_models) {
			DI->get('repo')->schema->save($model);
		}

		DI->get('repo')->actor_unit->save($actor);
		my $loaded = DI->get('repo')->actor_unit->load($actor->character->id);
		is_deeply $loaded, $actor, 'repository stored actor data ok';
	}
);

done_testing;
