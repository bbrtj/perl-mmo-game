package Game::Unit::BattleActor;

use header;
use Moo;
use Game::Types qw(InstanceOf);

no header;

extends 'Game::Unit::Actor';

has 'contestant' => (
	is => 'rw',
	isa => InstanceOf ['Game::Model::BattleContestant'],
);

1;
