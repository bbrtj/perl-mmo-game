package Unit::BattleActor;

use My::Moose;
use Types;

use header;

extends 'Unit::Actor';

has 'contestant' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::BattleContestant'],
);

sub models ($self)
{
	return [
		$self->SUPER::models->@*,
		$self->contestant,
	];
}

