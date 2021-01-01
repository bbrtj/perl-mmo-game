package Game::Mechanics::Battle::Affected;

use header;
use Game::Mechanics::Battle::Target;
use Game::Mechanics::Distance;
use List::Util qw(uniq);

sub affects ($self, $actor, $ability, $tactor)
{
	my ($cont, $char) = ($actor->contestant, $actor->character);
	my ($tcont, $tchar) = ($tactor->contestant, $tactor->character);

	return ($tcont->team == $cont->team && $ability->affect_ally) ||
		($tcont->team != $cont->team && $ability->affect_enemy);
}

sub get_affected ($self, $battle, $actor, $ability, $target)
{
	my @targets;

	my $tactor = Game::Mechanics::Battle::Target->get_actor($target);
	if (defined $tactor) {
		push @targets, $actor
			if $self->affects($actor, $ability, $tactor);
	}

	my $pos = Game::Mechanics::Battle::Target->get_position($battle, $target);
	push @targets, grep { $self->affects($actor, $ability, $_) }
		Game::Mechanics::Distance->find_aoe($battle, $ability, $pos);

	return [uniq @targets];
}

1;

