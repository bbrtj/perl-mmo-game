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
	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => $class->id,
		name => ucfirst lc $character_data->{name},
		base_stats => '',    # TODO
	);

	my $character_variables = Model::CharacterVariables->new(
		id => $character->id,
		location_id => Game::Config->config->{starting_location}->id,
		pos_x => Game::Config->config->{starting_location_x},
		pos_y => Game::Config->config->{starting_location_y},
		health => $class->data->define->{base_health},
		energy => $class->data->define->{base_energy},
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

