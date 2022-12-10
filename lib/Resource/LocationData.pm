package Resource::LocationData;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

use constant type => 'location_data';

sub generate ($self)
{
	return {
		id => $self->subject->id,
		name => $self->subject->name,
	};
}

