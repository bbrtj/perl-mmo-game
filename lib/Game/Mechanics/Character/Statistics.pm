package Game::Mechanics::Character::Statistics;

use Game::Config;
use Game::Character::Statistic::Stamina;
use Game::Character::Statistic::Speed;

use header;

sub get_current_level ($self, $actor)
{
	my $exp = $actor->variables->experience;
	my $first_level = 100;
	my $level = $exp >= $first_level + 1
		? int((log($exp / $first_level) / log(2) / 2)**(100 / 57)) + 1
		: 1;

	return $level;
}

sub get_max_health ($self, $actor)
{
	my $level = $self->get_current_level($actor);
	my $class = $actor->get_class;

	return $class->base_health
		+ $level * $class->health_per_level
		+ $actor->get_statistic(Game::Character::Statistic::Stamina) *
		Game::Character::Statistic::Stamina->primary_bonus;
}

sub get_battle_speed ($self, $actor)
{
	return $actor->get_class->base_speed
		* (
			1
			+ ($actor->get_statistic(Game::Character::Statistic::Speed) - Game::Config->secondary_stat_break_even)
			* Game::Character::Statistic::Speed->secondary_bonus
		);
}

