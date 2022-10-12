package Unit::Location;

use My::Moose;
use Model;
use Unit::Actor;
use Game::Helpers;

use header;

extends 'Unit';

has 'actors' => (
	is => 'rw',
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::Actor']],
);

has 'location' => (
	is => 'ro',
	isa => Types::InstanceOf ['Game::Lore::Location'],
	required => 1,
);

sub models ($self)
{
	return [
		map { $_->models->@* } $self->actors->@*,
	];
}

