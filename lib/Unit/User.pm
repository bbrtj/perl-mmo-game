package Unit::User;

use My::Moose;
use Model;
use Unit::Nested::Player;

use header;

extends 'Unit';

has param 'user' => (
	isa => Types::InstanceOf ['Model::User'],
);

has param 'players' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::Nested::Player']],
);

sub models ($self)
{
	return [
		$self->user,
		map { $_->models->@* } $self->players->@*,
	];
}

