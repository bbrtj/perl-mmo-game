package Resource::ActorMovementStopped;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

use constant type => 'actor_movement_stopped';

sub generate ($self)
{
	my $actor = $self->subject;

	return {
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
	};
}

