package Factory::Battle;

use My::Moose;
use Unit::Battle;

use header;

sub create ($self, $battle_result)
{
	my @contestant_results = $battle_result->contestants;
	my @contestants = map { Factory::Actor->create($_) } @contestant_results;

	return Unit::Battle->new(
		battle => $battle_result->to_model,
		contestants => \@contestants,
	);
}

