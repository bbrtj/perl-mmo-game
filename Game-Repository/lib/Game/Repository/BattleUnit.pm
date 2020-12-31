package Game::Repository::BattleUnit;

use header;
use Moo;
use Game::Common::Container;
use Game::Types qw(InstanceOf);
use Game::Unit::Battle;
use Game::Exception::RecordDoesNotExist;

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $unit)
{
	state $type_check = InstanceOf ['Game::Unit::Battle'];
	$type_check->assert_valid($unit);

	my $schema_repo = resolve('repo')->schema;

	$schema_repo->save($unit->battle);
	for my $actor ($unit->contestants->@*) {
		$schema_repo->save($actor->contestant);
	}

	return;
}

sub load ($self, $id)
{
	my $battle_result = resolve('dbc')->resultset('Battle')->search(
		{'battle.id' => $id},
		{
			prefetch => {contestants => {character => [qw(player variables contestant)]}}
		}
	)->single;

	Game::Exception::RecordDoesNotExist->throw
		unless defined $battle_result;

	my $actor_repo = resolve('repo')->actor_unit;
	my @contestant_results = $battle_result->contestants;
	my @contestants = map { $actor_repo->load($_->character_id, $_->character) } @contestant_results;

	return Game::Unit::Battle->new(
		battle => $battle_result->to_model,
		contestants => \@contestants,
	);
}

1;
