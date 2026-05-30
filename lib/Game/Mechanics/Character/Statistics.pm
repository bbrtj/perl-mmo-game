package Game::Mechanics::Character::Statistics;

use Game::Config;

use header;

use constant FIRST_LEVEL => 100;

sub get_current_level ($self, $exp)
{
	return 1 unless $exp >= FIRST_LEVEL;
	my $level_approx = int((log($exp / (FIRST_LEVEL / 2)) / log(2) / 2)**(100 / 58)) + 2;
	my $next_level_exp = $self->get_exp_for_level($level_approx + 1);

	return $level_approx + ($exp >= $next_level_exp);
}

sub get_exp_for_level ($self, $level)
{
	return ($level - 1) * FIRST_LEVEL if $level < 3;
	my $result = int(FIRST_LEVEL / 2 * 2**(2 * ($level - 2)**(58 / 100)));
	my $magnitude = int(log($result) / log 10);
	my $precision = 10**int($magnitude / 2);

	return int($result / $precision) * $precision;
}

