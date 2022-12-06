# HARNESS-CATEGORY-IMMISCIBLE

use Test2::Tools::E2ETest;
use Game::TestClient;
use Game::TestClientBag;
use ActorTest;
use Utils;

use all 'Resource';

use testheader;

use constant CLIENTS_COUNT => 3;

e2e_test {

	my $bag = Game::TestClientBag->new;
	my @player_ids;
	foreach my $client_n (1 .. CLIENTS_COUNT) {
		my $password = 'Testpassword123#';
		my ($actor, %related_models) = ActorTest->save_actor(password => $password);
		push @player_ids, $related_models{player}->id;

		$bag->add_client(
			Game::TestClient->new(actor => $actor)
				->add_action('Login', user => $related_models{user}, password => $password)
				->add_action('EnterGame')
		);
	}

	foreach my $key (keys @player_ids) {
		my @ids = @player_ids;
		splice @ids, $key, 1;

		$bag->clients->[$key]->add_action('State', received => {discovery => {'+actors' => \@ids}});
	}

	$bag->run;
};

done_testing;

