package Unit::User;

use My::Moose;

use header;

extends 'Unit';

has param 'user' => (
	isa => InstanceOf ['Model::User'],
);

has param 'players' => (
	isa => ArrayRef [InstanceOf ['Unit::Nested::Player']],
);

sub models ($self)
{
	return [
		$self->user,
		map { $_->models->@* } $self->players->@*,
	];
}

