package Repository::BattleUnit;

use Moo;
use DI;
use Types;
use Unit::Battle;
use Exception::RecordDoesNotExist;

use header;

with 'Repository::Role::Resource';

sub save ($self, $unit)
{
	state $type_check = Types::InstanceOf ['Unit::Battle'];
	$type_check->assert_valid($unit);

	my $schema_repo = DI->get('schema_repo');

	$schema_repo->save($unit->battle);
	for my $actor ($unit->contestants->@*) {
		$schema_repo->save($actor->contestant);
	}

	return;
}

sub load ($self, $id)
{
	my $battle_result = DI->get('db')->dbc->resultset('Battle')->search(
		{'battle.id' => $id},
		{
			prefetch => {contestants => {character => [qw(player variables contestant)]}}
		}
	)->single;

	Exception::RecordDoesNotExist->throw
		unless defined $battle_result;

	my $actor_repo = DI->get('actor_unit');
	my @contestant_results = $battle_result->contestants;
	my @contestants = map { $actor_repo->load($_->character_id, $_->character) } @contestant_results;

	return Unit::Battle->new(
		battle => $battle_result->to_model,
		contestants => \@contestants,
	);
}

