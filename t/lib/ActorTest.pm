package ActorTest;

use Game::Helpers;
use Game::Config;
use Mojo::JSON qw(from_json to_json);
use all 'Resource';

use all 'Model', 'Unit';

use header;

sub save_actor ($self, @params)
{
	my ($actor, %related_models) = $self->create_actor(@params);

	foreach my $model (@related_models{qw(user player character)}) {
		DI->get('models_repo')->save($model);
	}

	# insert, not update
	DI->get('units_repo')->save($actor, 0);

	return ($actor, %related_models);
}

sub create_actor ($self, $password = 'asdfasdf')
{
	my $faker = DI->get('faker_service');

	my $user = $faker->fake_user($password);
	my $player = $faker->fake_player($user->id);
	my $character = $faker->fake_character($player->id);
	my $variables = $faker->fake_variables($character->id);

	return (
		Unit::Actor->new(
			player => $player,
			character => $character,
			variables => $variables,
		),
		user => $user,
		player => $player,
		character => $character,
		variables => $variables,
	);
}

sub actor_server_login_data ($self, $actor, $user, $password)
{
	my @send_queue = (
		'1;login;' . to_json({email => $user->email, password => $password}),
		'2;list_characters',
		'3;enter_game;' . $actor->player->id,
	);

	my @receive_queue = (
		['1', '1'],
		['2', Resource::CharacterList->new(DI->get('units_repo')->load_user($user->id))->serialize],
		['3', '1'],
		[
			'',
			Resource::LocationData->new(DI->get('lore_data_repo')->load($actor->variables->location_id))
				->serialize
		],
	);

	return (\@send_queue, \@receive_queue);
}

