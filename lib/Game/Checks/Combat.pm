package Game::Checks::Combat;

use Exporter qw(import);
use X::Pub::CheckFailed;

use header;

our @EXPORT_OK = qw(
	can_use_ability
);

sub can_use_ability ($actor, $lore, $x, $y)
{
	# basic checks to make sure we are not getting fooled
	X::Pub->raise
		unless $actor && $lore;

	# another action is in progress already
	X::Pub::CheckFailed->raise(Err::ACTION_IN_PROGRESS)
		if $actor->stats->has_action;

	# bad ability
	X::Pub::CheckFailed->raise(Err::INVALID_ACTION)
		unless $lore isa 'Game::Lore::Ability';

	# TODO: cooldown
	# TODO: not enough energy
	# TODO: no control over the character (CC)

	return;
}

