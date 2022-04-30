package Resource::MapData;

use My::Moose;

use header;

extends 'Resource';

has '+subject' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

sub serialize ($self)
{
	return {
		coordinates => $self->subject->data->map->coordinates,
	};
}

