package Service::Character;

use My::Moose;
use all 'Model';
use Game::Config;

use header;

has injected 'models_repo';
has injected 'lore_data_repo';

# $character_data should be validated
sub create_character ($self, $user, $character_data)
{
	my $player = Model::Player->new(user_id => $user->id);

	my $class = $self->lore_data_repo->load($character_data->{class_id});
	my $race = $self->lore_data_repo->load($character_data->{race_id});
	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => $class->id,
		race_id => $race->id,
		alliance_id => Game::Config->starting_alliance->id,
		name => ucfirst lc $character_data->{name},
	);

	my $character_variables = Model::CharacterVariables->new(
		id => $character->id,
		location_id => Game::Config->starting_location->{location}->id,
		pos_x => Game::Config->starting_location->{'x'},
		pos_y => Game::Config->starting_location->{'y'},
		health => Game::Config->base_health,
		energy => Game::Config->base_energy,
	);

	$self->models_repo->db->transaction(
		sub {
			$self->models_repo->save($player);
			$self->models_repo->save($character);
			$self->models_repo->save($character_variables);
		}
	);

	return $player;
}

