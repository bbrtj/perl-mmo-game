# HARNESS-CATEGORY-IMMISCIBLE

# disable randomness before Game::Mechanics::Rng compiles
BEGIN { $ENV{TEST_NO_RANDOM} = 1; }

use Test2::Tools::E2ETest;
use Test2::Tools::Compare qw(number_gt);
use Game::TestClient;
use Game::Helpers;
use Game::TestClientBag;
use Game::Mechanics::Generic qw(calculate_angle);
use ActorTest;

use all 'Game::TestClient::Resource';

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
				pos_x => 7,
				pos_y => 7,
			}
		);

		my ($friendly, %friendly_related_models) = ActorTest->save_actor(
			password => $password,
			character_params => {
				alliance_id => 'alli.colon',
			},
			variables_params => {
				location_id => 'loc.cp_harbor',
				pos_x => 8.5,
				pos_y => 8.5,
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
		DI->get('models_repo')->update($friendly_related_models{variables});
		DI->get('models_repo')->update($dummy_related_models{variables});

		my $ability = lore_ability 'Shoot';
		$bag->add_client(
			Game::TestClient->new(name => 'actor', actor => $actor)
				->add_action('Login', user => $related_models{user}, password => $password)
				->add_action('EnterGame')
				->add_action(
					'State',
					received => {
						'new_actors' => [$friendly->id, $dummy->id]
					},
					types => ['discovery'],
				)
				->add_action(
					'Feed',
					data => [
						Resource::ActorPosition->new(subject => $dummy),
						Resource::ActorState->new(subject => $dummy),
						Resource::ActorPosition->new(subject => $friendly),
						Resource::ActorState->new(subject => $friendly),
					],
				)
				->add_action('UseAbility', actor => $actor, lore => $ability, x => 10, y => 10)
		);

		my $actor_action = Game::Object::Action::Ability->new(
			x => 10,
			y => 10,
			lore => $ability,
			actor => $actor,
			duration => $ability->speed_multiplier,
		);

		my $projectile = Game::Object::Projectile->new(
			effect => Game::Object::Effect::Damage->new(
				actor => $actor,
				lore => $ability,
			),
			angle => calculate_angle($actor->variables->xy, 10, 10),
			speed => $ability->projectile->{speed},
			max_distance => $ability->projectile->{range},
		);

		my $projectile_stop = Game::TestClient::Resource::ProjectileStop->new(
			subject => $projectile,
		);

		$bag->add_client(
			Game::TestClient->new(name => 'friendly', actor => $friendly)
				->add_action('Login', user => $friendly_related_models{user}, password => $password)
				->add_action('EnterGame')
				->add_action(
					'Feed',
					data => [
						Resource::ActorAction->new(
							subject => $actor_action,
						),
						Game::TestClient::Resource::Projectile->new(
							subject => $projectile,
							stop_resource => $projectile_stop,
						),
						$projectile_stop,
					],
				)
		);

		$bag->add_client(
			Game::TestClient->new(name => 'dummy', actor => $dummy)
				->add_action('Login', user => $dummy_related_models{user}, password => $password)
				->add_action('EnterGame')
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

