package Game::Mechanics::Battle::Action;

use header;

sub ends_turn ($self, $actor, %action)
{
	if (exists $action{ability}) {
		return !$action{ability}->instant;
	}
	elsif (exists $action{movement}) {

		# TODO: calculate movement and check if it should end turn
		return 1;
	}

	return 0;
}

