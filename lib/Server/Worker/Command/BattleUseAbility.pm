package Server::Worker::Command::BattleUseAbility;

use My::Moose;
use DI;
use Game::Ability::Active;
use Game::Mechanics::Check::Ability;
use Game::Mechanics::Battle::Affected;
use Game::Mechanics::Battle::Action;
use Exception::RecordDoesNotExist;

use header;

extends 'Server::Worker::Command';

use constant name => 'battle_use_ability';

sub handle ($self, $job, $battle_id, $caster_id, $ability_id, $target)
{
	# make sure we have no concurrent jobs for that battle
	my $repo = DI->get('battle_unit');
	my $battle = $repo->load($battle_id);
	my $actor = $battle->find_contestant($caster_id);
	my $ability = Game::Ability::Active->get($ability_id);

	Exception::RecordDoesNotExist->throw
		unless defined $actor && defined $ability;

	# try-catch this so that we can handle asserts
	# check if the caster has turn now
	# check if the caster can use that ability
	Game::Mechanics::Check::Ability->valid_target($battle, $actor, $ability, $target)->assert_valid;
	Game::Mechanics::Check::Ability->in_range($battle, $actor, $ability, $target)->assert_valid;

	# check mana, cooldown

	my $affected = Game::Mechanics::Battle::Affected->get_affected($battle, $actor, $ability, $target);
	$ability->effects($actor, $affected);

	if (Game::Mechanics::Battle::Action->ends_turn($actor, ability => $ability)) {

		# enqueue end of turn
	}

	$repo->save($battle);

	# setup feedback for the web application
	return;
}

