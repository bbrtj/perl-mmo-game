package Repository::BattleUnit;

use Moo;
use Types;
use Unit::Battle;
use Exception::RecordDoesNotExist;

use header;

with 'Repository::Role::Resource';

has 'repo' => (
	is => 'ro',
);

has 'db' => (
	is => 'ro',
);

has 'actor_repo' => (
	is => 'ro',
);

sub save ($self, $unit)
{
	state $type_check = Types::InstanceOf ['Unit::Battle'];
	$type_check->assert_valid($unit);

	$self->repo->save($unit->battle);
	for my $actor ($unit->contestants->@*) {
		$self->repo->save($actor->contestant);
	}

	return;
}

sub load ($self, $id)
{
	my $battle_result = $self->db->dbc->resultset('Battle')->search(
		{'battle.id' => $id},
		{
			prefetch => {contestants => {character => [qw(player variables contestant)]}}
		}
	)->single;

	Exception::RecordDoesNotExist->throw
		unless defined $battle_result;

	my @contestant_results = $battle_result->contestants;
	my @contestants = map { $self->actor_repo->load($_->character_id, $_->character) } @contestant_results;

	return Unit::Battle->new(
		battle => $battle_result->to_model,
		contestants => \@contestants,
	);
}

