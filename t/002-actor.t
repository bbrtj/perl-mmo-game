use v5.32;
use warnings;

use Test::More;
use Test::Exception;
use Game::Common::Container;
use lib 't/lib';
use DatabaseTest;
use ActorTest;

DatabaseTest->test(
	sub {
		my ($actor, @related_models) = ActorTest->create_actor;
		foreach my $model (@related_models) {
			resolve('repo')->schema->save($model);
		}

		resolve('repo')->actor_unit->save($actor);
		my $loaded = resolve('repo')->actor_unit->load($actor->character->id);
		is_deeply $loaded, $actor;
	}
);

done_testing;
