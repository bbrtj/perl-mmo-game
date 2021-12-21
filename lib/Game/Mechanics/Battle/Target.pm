package Game::Mechanics::Battle::Target;

use Exception::InvalidTarget;

use header;

sub get_actor ($self, $battle, $target)
{
	return $battle->find_contestant($target);
}

sub get_position ($self, $battle, $target)
{
	return $target
		if ref $target eq ref [] && @$target == 2;

	my $actor = $self->get_actor($battle, $target);
	Exception::InvalidTarget->throw unless defined $actor;

	return [$actor->contestant->pos_x, $actor->contestant->pos_y];
}

sub valid_target ($self, $battle, $actor, $ability, $target)
{
	my $tactor = $self->get_actor($battle, $target);

	if ($tactor) {

		my ($cont, $char) = ($actor->contestant, $actor->character);
		my ($tcont, $tchar) = ($tactor->contestant, $tactor->character);

		return ($tchar->id eq $char->id && $ability->target_self) ||
			($tcont->team == $cont->team && $ability->target_ally) ||
			($tcont->team != $cont->team && $ability->target_enemy);
	}
	else {
		return $ability->target_ground;
	}
}

