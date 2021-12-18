package Game::Repository::ActorUnit;

use Moo;
use DI;
use Types;
use Game::Unit::Actor;
use Game::Unit::BattleActor;
use Game::Exception::RecordDoesNotExist;

use header;

with 'Game::Repository::Role::Resource';

sub save ($self, $unit)
{
	state $type_check = Types::InstanceOf ['Game::Unit::Actor'];
	$type_check->assert_valid($unit);

	my $schema_repo = DI->get('schema_repo');

	# we do not save player / npc / contestant here on purpose
	$schema_repo->save($unit->character);
	$schema_repo->save($unit->variables);

	return;
}

sub load ($self, $id, $char_result = undef)
{
	if (!defined $char_result) {
		$char_result = DI->get('db')->dbc->resultset('Character')->search(
			{'me.id' => $id},
			{
				prefetch => [qw(player variables contestant)],
			}
		)->single;
	}

	Game::Exception::RecordDoesNotExist->throw
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
		? Game::Unit::Actor::
		: Game::Unit::BattleActor::
		;

	return $class->new(%args);
}

