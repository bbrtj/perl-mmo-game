package ActorTest;

use Game::Helpers;
use Game::Config;

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

