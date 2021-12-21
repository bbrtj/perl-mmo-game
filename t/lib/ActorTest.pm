package ActorTest;

use Game::Unit::Actor;
use Game::Model::Character;
use Game::Model::CharacterVariables;
use Game::Model::Player;
use Game::Model::User;
use Game::Character::Class::Priest;

use header;

sub create_actor ($self)
{
	my $user = Game::Model::User->dummy->new(
		email => 'test@test.pl',
	);
	$user->set_password('asdfasdf');
	$user->promote;

	my $player = Game::Model::Player->new(
		user_id => $user->id,
	);

	my $character = Game::Model::Character->new(
		player_id => $player->id,
		class_id => Game::Character::Class::Priest->lore_id,
		name => 'Priestx',
		stats => 'STT_CHA:10',
	);

	my $variables = Game::Model::CharacterVariables->new(
		id => $character->id,
		experience => 1234,
		location => 'LOC_GMDUNG',
		health => 100.3,
		mana => 120.6,
	);

	return (
		Game::Unit::Actor->new(
			player => $player,
			character => $character,
			variables => $variables,
		),
		$user,
		$player
	);
}

