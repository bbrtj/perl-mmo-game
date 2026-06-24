# HARNESS-CATEGORY-IMMISCIBLE

use Test2::Tools::E2ETest;
use Test2::Tools::Compare qw(number_gt);
use Game::TestClient;
use Game::Helpers;
use Game::TestClientBag;
use ActorTest;

use testheader;

my $dummy_variables;

DI->get('env')->setenv(TEST_NO_REGENERATION => true);
e2e_test(
	sub {

		my $bag = Game::TestClientBag->new;

		my $password = 'Testpassword123#';
		my ($actor, %related_models) = ActorTest->save_actor(
			password => $password,
			character_params => {
				alliance_id => 'alli.colon',
			},
			variables_params => {
				location_id => 'loc.cp_harbor',
				pos_x => 9,
				pos_y => 9,
			}
		);

		my ($dummy, %dummy_related_models) = ActorTest->save_actor(
			password => $password,
			variables_params => {
				location_id => 'loc.cp_harbor',
				pos_x => 10,
				pos_y => 10,
			}
		);

		DI->get('models_repo')->update($related_models{variables});
		DI->get('models_repo')->update($dummy_related_models{variables});

		$bag->add_client(
			Game::TestClient->new(actor => $actor)
				->add_action('Login', user => $related_models{user}, password => $password)
				->add_action('EnterGame')
				->add_action('Move', x => 9.6, y => 9.6)
				->add_action(
					'State',
					received => {
						'new_actors' => [$dummy->id]
					},
					types => ['discovery'],
				)
				->add_action(
					'Feed',
					data => [
						Resource::ActorPosition->new(subject => $dummy),
						Resource::ActorState->new(subject => $dummy),
					],
				)
				->add_action('UseAbility', actor => $actor, lore => lore_ability 'Strike')
		);

		# TODO: real duration
		# NOTE: x/y are required, but they do not change anything with this type of ability
		my $actor_action = Game::Object::Action::Ability->new(
			x => 5,
			y => 3,
			lore => lore_ability 'Strike',
			actor => $actor,
			duration => 1,
		);

		$bag->add_client(
			Game::TestClient->new(actor => $dummy)
				->add_action('Login', user => $dummy_related_models{user}, password => $password)
				->add_action('EnterGame')
				->add_action(
					'Feed',
					data => [
						Resource::ActorAction->new(
							subject => $actor_action,
						),
					],
				)
		);

		$bag->run;
		$dummy_variables = $dummy->variables;
	},
	sub {
		die 'no dummy variables'
			unless defined $dummy_variables;

		my $dummy_health = DI->get('models_repo')->load(CharacterVariables => $dummy_variables->id)->health;
		my $damage = $dummy_variables->health - $dummy_health;

		# TODO: check for damage amount
		is $damage, number_gt(0.01), 'damage taken ok';
		note sprintf 'damage taken was: %f', $damage;
	},
);

done_testing;

