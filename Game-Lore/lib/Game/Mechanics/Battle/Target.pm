package Game::Mechanics::Battle::Target;

use header;

sub is_character ($self, $battle, $target)
{
	return $battle->find_contestant($target);
}

sub get_position ($self, $battle, $target)
{
	return $target
		if ref $target eq ref [] && @$target == 2;

	my $contestant = $self->is_character($battle, $target);
	die 'invalid target' unless defined $contestant;

	return [$contestant->[0]->pos_x, $contestant->[0]->pos_y];
}

1;

