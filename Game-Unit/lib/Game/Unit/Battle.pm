package Game::Unit::Battle;

use header;
use Moo;
use Game::Model::Battle;
use Game::Model::BattleContestant;
use Game::Types qw(InstanceOf ArrayRef Tuple);
use List::Util qw(first);

no header;

with 'Game::Unit';

has 'battle' => (
	is => 'rw',
	isa => InstanceOf ['Game::Model::Battle'],
);

has 'contestants' => (
	is => 'rw',
	isa => ArrayRef [Tuple [InstanceOf ['Game::Model::BattleContestant'], InstanceOf ['Game::Unit::Actor']]],
);

sub find_contestant ($self, $id)
{
	return first { $_->[1]->character->id eq $id }
		$self->contestants->@*;
}

1;

