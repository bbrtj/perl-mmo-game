package Factory::Actor;

use My::Moose;
use Unit::Actor;
use Schema::Utils qw(fetch_single);

use header;

extends 'Factory';

sub fetch ($self, $character_id)
{
	my $rs = $self->dbc->resultset('Character')->search(
		{'me.id' => $character_id},
		{
			prefetch => [qw(player variables)],
		}
	);

	return fetch_single($rs);
}

sub create ($self, $character_result)
{
	my $player_result = $character_result->player;

	# my $npc_result = $character_result->npc;
	my $variables = $character_result->variables->to_model;

	return Unit::Actor->new(
		($player_result ? (player => $player_result->to_model) : ()),

		# ($npc_result ? (npc => $npc_result->to_model) : ()),
		character => $character_result->to_model,
		variables => $variables,
	);
}

