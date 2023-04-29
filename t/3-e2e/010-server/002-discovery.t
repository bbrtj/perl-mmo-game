# HARNESS-CATEGORY-IMMISCIBLE

use Test2::Tools::E2ETest;
use Game::TestClient;
use Game::TestClientBag;
use ActorTest;
use Utils;

use all 'Resource';

use testheader;

use constant CLIENTS_COUNT => 10;

e2e_test {

	my $bag = Game::TestClientBag->new;
	my @players;
	foreach my $client_n (1 .. CLIENTS_COUNT) {
		my $password = 'Testpassword123#';
		my ($actor, %related_models) = ActorTest->save_actor(password => $password);
		push @players, $actor;

		$bag->add_client(
			Game::TestClient->new(actor => $actor)
				->add_action('Login', user => $related_models{user}, password => $password)
				->add_action('EnterGame')
		);
	}

	foreach my $key (keys @players) {
		my @others = @players;
		splice @others, $key, 1;

		@others = map {
			+{
				id => $_->id,
				x => $_->variables->pos_x,
				y => $_->variables->pos_y,
			}
		} @others;

		$bag->clients->[$key]->add_action(
			'State',
			received => {'new_actors' => \@others},
			types => ['discovery'],
		);
	}

	$bag->run;
};

done_testing;

