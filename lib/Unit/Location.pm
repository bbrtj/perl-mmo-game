package Unit::Location;

use My::Moose;
use Model;
use Unit::Actor;
use Game::Helpers;

# use Sub::HandlesVia;

use header;

extends 'Unit';

has param 'actors' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::Actor']],
	'handles[]' => {
		'add_actor' => 'push',
	}
);

has param 'location' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

sub models ($self)
{
	return [
		map { $_->models->@* } $self->actors->@*,
	];
}

