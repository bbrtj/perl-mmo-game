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
	my $character = Game::Model::Character->dummy->new;
	my $variables = Game::Model::CharacterVariables->dummy->new(id => $character->id);
	my $player = Game::Model::Player->dummy->new;
	my $user = Game::Model::User->dummy->new;

	$user->set_email('test@test.pl');
	$user->set_password('asdfasdf');
	$user->promote;

	$player->set_user_id($user->id);
	$player->promote;

	$character->set_player_id($player->id);
	$character->set_class_id(Game::Character::Class::Priest->lore_id);
	$character->set_name('Priestx');
	$character->set_stats('STT_CHA:10');
	$character->promote;

	$variables->set_experience(1234);
	$variables->set_location('LOC_GMDUNG');
	$variables->set_health(100.3);
	$variables->set_mana(120.6);
	$variables->promote;

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

