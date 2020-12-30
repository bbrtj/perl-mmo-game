package Game::Repository::ActorUnit;

use header;
use Moo;
use Game::Common::Container;
use Game::Types qw(InstanceOf);
use Game::Unit::Actor;
use Game::Unit::BattleActor;

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $unit)
{
	state $type_check = InstanceOf ['Game::Unit::Actor'];
	$type_check->assert_valid($unit);

	my $schema_repo = resolve('repo')->schema;

	# we do not save player / npc here on purpose
	$schema_repo->save($unit->character);
	$schema_repo->save($unit->variables);
	if ($unit->isa(Game::Unit::BattleActor::)) {
		$schema_repo->save($unit->contestant);
	}
}

sub load ($self, $id)
{
	my $char_result = resolve('dbc')->resultset('Character')->single({id => $id});
	my $player_result = $char_result->player;

	# my $npc_result = $char_result->npc;
	my $variables = $char_result->variables->to_model;
	my $contestant_result = $char_result->contestant;

	my %args = (
		($player_result ? player => $player_result->to_model : ()),

		# ($npc_result ? npc => $npc_result->to_model : ()),
		($contestant_result ? contestant => $contestant_result->to_model : ()),
		character => $char_result->to_model,
		variables => $variables,
	);

	$class = $contestant_result
		? Game::Unit::Actor::
		: Game::Unit::BattleActor::
		;

	return $class->new(%args);
}

1;
