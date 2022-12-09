package Resource::LocationData;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

sub _serialize ($self)
{
	return {
		location => $self->subject->name,
	};
}

