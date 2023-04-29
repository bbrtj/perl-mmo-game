package Resource::LocationData;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

has param 'actor' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

use constant type => 'location_data';

sub generate ($self)
{
	return {
		id => $self->subject->id,
		player_x => $self->actor->variables->pos_x,
		player_y => $self->actor->variables->pos_y,
	};
}

