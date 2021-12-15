package Game::Mechanics::RNG;

use Game::RNG;
use Game::Config;
use Game::Character::Statistic::Luck;

use header;

sub roll ($self, $chance)
{
	return rng < $chance;
}

sub roll_with_luck ($self, $chance, $actor)
{
	return $self->roll(
		$chance
			* (
				1
				+ ($actor->get_statistic(Game::Character::Statistic::Luck) - Game::Config->secondary_stat_break_even)
				* Game::Character::Statistic::Luck->secondary_bonus
			)
	);
}

