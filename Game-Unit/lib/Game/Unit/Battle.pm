package Game::Unit::Battle;

use header;
use Moo;
use Game::Model::Battle;
use Game::Model::BattleContestant;
use Game::Types qw(InstanceOf ArrayRef Tuple);

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

1;

