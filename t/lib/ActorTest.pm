package ActorTest;

use Unit::Actor;
use Model::Character;
use Model::CharacterVariables;
use Model::Player;
use Model::User;
use Game::Character::Class::Priest;

use header;

sub create_actor ($self)
{
	my $user = Model::User->new(
		email => 'test@test.pl',
	);
	$user->set_password('asdfasdf');
	$user->promote;

	my $player = Model::Player->new(
		user_id => $user->id,
	);

	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => Game::Character::Class::Priest->lore_id,
		name => 'Priestx',
		stats => 'STT_CHA:10',
	);

	my $variables = Model::CharacterVariables->new(
		id => $character->id,
		experience => 1234,
		location => 'LOC_GMDUNG',
		health => 100.3,
		mana => 120.6,
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

