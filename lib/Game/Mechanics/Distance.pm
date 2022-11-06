package Game::Mechanics::Distance;

use header;

sub is_in_range ($self, $pos1, $pos2, $range)
{
	my ($sx, $sy, $ex, $ey) = (@$pos1, @$pos2);

	return sqrt(($sx - $ex)**2 + ($sy - $ey)**2) <= $range;
}

sub find_actors_in_range ($self, $server, $actor, $range)
{
	my $variables = $actor->variables;
	my @found = $server->find_in_radius($variables->pos_x, $variables->pos_y, $range)->@*;
	return [grep { $_ ne $actor } @found];
}

