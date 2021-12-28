package Factory::Actor;

use My::Moose;
use Unit::Actor;
use Unit::BattleActor;

use header;

sub create ($self, $character_result)
{
	my $player_result = $character_result->player;

	# my $npc_result = $character_result->npc;
	my $variables = $character_result->variables->to_model;
	my $contestant_result = $character_result->contestant;

	my %args = (
		($player_result ? (player => $player_result->to_model) : ()),

		# ($npc_result ? (npc => $npc_result->to_model) : ()),
		($contestant_result ? (contestant => $contestant_result->to_model) : ()),
		character => $character_result->to_model,
		variables => $variables,
	);

	my $class = $contestant_result
		? 'Unit::Actor'
		: 'Unit::BattleActor'
		;

	return $class->new(\%args);
}

