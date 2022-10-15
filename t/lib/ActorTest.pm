package ActorTest;

use Unit::Actor;
use Model::Character;
use Model::CharacterVariables;
use Model::Player;
use Model::User;
use Game::Helpers;

use header;

sub create_actor ($self)
{
	my $user = Model::User->new(
		-dummy,
		email => 'test@test.pl',
	);
	$user->set_password('asdfasdf');
	$user->promote;

	my $player = Model::Player->new(
		user_id => $user->id,
	);

	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => lore_class('Witchhunter')->id,
		name => 'Whx',
		base_stats => '',
	);

	my $variables = Model::CharacterVariables->new(
		id => $character->id,
		experience => 1234,
		location_id => 'LOC_GMDUNG',
		pos_x => 2.2,
		pos_y => 5.3,
		health => 100.3,
		energy => 120.6,
	);

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

