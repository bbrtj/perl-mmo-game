package Resource::LocationData;

use My::Moose;

use Resource::ActorEvent;

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

sub _build_next_resources ($self)
{
	return [
		Resource::ActorEvent->new(subject => $self->actor),
	];
}

