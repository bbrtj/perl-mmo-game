package Game::Mechanics::Character::Statistics;

use Game::Config;

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

