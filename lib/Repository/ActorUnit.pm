package Repository::ActorUnit;

use Moo;
use Types;
use Unit::Actor;
use Unit::BattleActor;
use Exception::RecordDoesNotExist;

use header;

with 'Repository::Role::Resource';

has 'repo' => (
	is => 'ro',
);

has 'db' => (
	is => 'ro',
);

sub save ($self, $unit)
{
	state $type_check = Types::InstanceOf ['Unit::Actor'];
	$type_check->assert_valid($unit);

	# we do not save player / npc / contestant here on purpose
	$self->repo->save($unit->character);
	$self->repo->save($unit->variables);

	return;
}

sub load ($self, $id, $char_result = undef)
{
	if (!defined $char_result) {
		$char_result = $self->db->dbc->resultset('Character')->search(
			{'me.id' => $id},
			{
				prefetch => [qw(player variables contestant)],
			}
		)->single;
	}

	Exception::RecordDoesNotExist->throw
		unless defined $char_result;

	my $player_result = $char_result->player;

	# my $npc_result = $char_result->npc;
	my $variables = $char_result->variables->to_model;
	my $contestant_result = $char_result->contestant;

	my %args = (
		($player_result ? (player => $player_result->to_model) : ()),

		# ($npc_result ? (npc => $npc_result->to_model) : ()),
		($contestant_result ? (contestant => $contestant_result->to_model) : ()),
		character => $char_result->to_model,
		variables => $variables,
	);

	my $class = $contestant_result
		? 'Unit::Actor'
		: 'Unit::BattleActor'
		;

	return $class->new(%args);
}

