package ActorTest;

use Game::Helpers;
use Game::Config;
use Test2::Tools::PrepareObjects;

use all 'Resource', 'Model', 'Unit';

use header;

sub save_actor ($self, @params)
{
	my ($actor, %related_models) = $self->create_actor(@params);

	$self->save_actor_models(%related_models);

	return ($actor, %related_models);
}

sub save_actor_models ($self, %models)
{
	foreach my $model (@models{qw(user player character variables)}) {
		DI->get('models_repo')->save($model);
	}

	return;
}

sub create_actor ($self, %params)
{
	%params = (
		password => 'asdfasdf',
		%params
	);

	my $faker = DI->get('faker_service');

	my $user = $faker->fake_user($params{password});
	my $player = $faker->fake_player($user->id);
	my $character = $faker->fake_character($player->id);
	my $variables = $faker->fake_variables($character->id, %{$params{variables_params} // {}});

	my %models = (
		user => $user,
		player => $player,
		character => $character,
		variables => $variables,
	);

	# prepare models for comparing with 'is'
	prepare($_) for values %models;

	return (
		Unit::Actor->new(
			player => $player,
			character => $character,
			variables => $variables,
		),
		%models
	);
}

