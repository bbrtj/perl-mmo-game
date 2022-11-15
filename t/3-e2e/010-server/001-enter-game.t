# HARNESS-CATEGORY-IMMISCIBLE

use Test2::Tools::E2ETest;
use Game::TestClient;
use Game::TestClientBag;
use ActorTest;
use Utils;

use testheader;

e2e_test {

	my $password = 'Testpassword123#';
	my ($actor, %related_models) = ActorTest->save_actor($password);

	my $bag = Game::TestClientBag->new;
	$bag->add_client(
		Game::TestClient->new(actor => $actor)
			->add_action('Login', user => $related_models{user}, password => $password)
			->add_action('ListCharacters')
			->add_action('EnterGame')
	);

	$bag->run;
};

done_testing;

