# HARNESS-CATEGORY-IMMISCIBLE

use Test2::Tools::E2ETest;
use Game::TestClient;
use Game::TestClientBag;
use ActorTest;
use Utils;

use testheader;

e2e_test {

	my $password = 'Testpassword123#';
	my ($actor, %related_models) = ActorTest->save_actor(
		password => $password,
		variables_params => {
			location_id => 'L.LOC.CP_HARBOR',
			pos_x => 1.3,
			pos_y => 1.2,
		}
	);

	DI->get('models_repo')->update($related_models{variables});

	my $bag = Game::TestClientBag->new;
	$bag->add_client(
		Game::TestClient->new(actor => $actor)
			->add_action('Login', user => $related_models{user}, password => $password)
			->add_action('EnterGame')
			->add_action('Move', x => 5.3, y => 2.2)
			->add_action('Stop')
			->add_action('Stop', was_moving => !!0)
	);

	$bag->run;
};

done_testing;

