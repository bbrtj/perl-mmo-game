package Service::Character;

use My::Moose;
use Model;
use Game::Config;

use header;

has injected 'repo' => as => 'models';

# $player_data should be validated (Form::CreatePlayer)
sub create_player ($self, $user, $player_data)
{
	my $player = Model::Player->new(user_id => $user->id);

	my $class = $player_data->{class};
	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => $class->id,
		name => ucfirst lc $player_data->{name},
		base_stats => '', # TODO
	);

	my $character_variables = Model::CharacterVariables->new(
		id => $character->id,
		location_id => Game::Config->config->{starting_location}->id,
		pos_x => Game::Config->config->{starting_location_x},
		pos_y => Game::Config->config->{starting_location_y},
		health => $class->data->define->{base_health},
		energy => $class->data->define->{base_energy},
	);

	$self->repo->db->transaction(
		sub {
			$self->repo->save($player);
			$self->repo->save($character);
			$self->repo->save($character_variables);
		}
	);

	return $player;
}

