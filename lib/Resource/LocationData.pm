package Resource::LocationData;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

sub serialize ($self)
{
	return {
		coordinates => $self->subject->data->map->coordinates,
	};
}

