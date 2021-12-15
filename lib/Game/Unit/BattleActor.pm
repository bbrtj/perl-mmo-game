package Game::Unit::BattleActor;

use Moo;
use Types;

use header;

extends 'Game::Unit::Actor';

has 'contestant' => (
	is => 'rw',
	isa => Types::InstanceOf ['Game::Model::BattleContestant'],
);

