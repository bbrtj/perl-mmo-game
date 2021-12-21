package Unit::BattleActor;

use Moo;
use Types;

use header;

extends 'Unit::Actor';

has 'contestant' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::BattleContestant'],
);

