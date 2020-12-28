package Game::Mechanics::RNG;

use header;
use Game::RNG;
use Game::Config;
use Game::Character::Statistic::Luck;

no header;

sub roll ($self, $chance)
{
	return rng < $chance;
}

sub roll_with_luck ($self, $chance, $actor)
{
	return $self->roll(
		$chance
			* ($actor->get_statistic(Game::Character::Statistic::Luck) - Game::Config->secondary_stat_break_even)
			* (1 + Game::Character::Statistic::Luck->secondary_bonus)
	);
}

1;

