package Game::Mechanics::Distance;

use header;

sub is_in_range ($self, $pos1, $pos2, $range)
{
	my ($sx, $sy, $ex, $ey) = (@$pos1, @$pos2);

	# TODO: vectors
	return sqrt(($sx - $ex)**2 + ($sy - $ey)**2) <= $range;
}

sub find_aoe ($self, $battle, $ability, $pos)
{
	return unless defined $ability->aoe;

	my @found;
	for my $actor ($battle->contestants) {
		my $pos2 = [$actor->contestant->pos_x, $actor->contestant->pos_y];
		push @found, $actor
			if $self->is_in_range($pos, $pos2, $ability->aoe);
	}

	return \@found;
}
