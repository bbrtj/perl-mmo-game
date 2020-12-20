package Game::Mechanics::Character::Statistics;

use header;

no header;

sub get_current_level ($self, $actor)
{
	my $exp = $actor->variables->experience;
	my $first_level = 100;
	my $level = $exp >= $first_level + 1
		? int((log($exp / $first_level) / log(2) / 2)**(100 / 57)) + 1
		: 1;

	return $level;
}

sub calc_statistics ($self, $actor)
{
	# TODO get the actual stats
}

sub get_max_health ($self, $actor)
{
	my $level = $self->get_current_level($actor);

	# TODO change this based on class
	my $base = 100;
	my $per_level = 15;

	# TODO replace constant with calculated stat
	my $stamina = 10;
	my $per_stamina = 5;

	return $base + $level * $per_level + $stamina * $per_stamina;
}

sub get_battle_speed ($self, $actor)
{
	# TODO replace constant with calculated stat
	my $speed = 10;
}

1;
