package Unit::User;

use My::Moose;
use Model;
use Unit::Nested::Player;

use header;

extends 'Unit';

has 'user' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::User'],
);

has 'players' => (
	is => 'rw',
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::Nested::Player']],
);

sub models ($self)
{
	return [
		$self->user,
		map { $_->models->@* } $self->players->@*,
	];
}

