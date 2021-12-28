package Unit::Battle;

use My::Moose;
use Model::Battle;
use Types;
use List::Util qw(first);

use header;

extends 'Unit';

has 'battle' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Battle'],
);

has 'contestants' => (
	is => 'rw',
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::BattleActor']],
);

sub find_contestant ($self, $id)
{
	return first { $_->character->id eq $id }
		$self->contestants->@*;
}

sub models ($self)
{
	return [
		$self->battle,
		map { $_->models->@* } $self->contestants->@*,
	];
}

