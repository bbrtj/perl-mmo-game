package Resource::ActorMovement;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

use constant type => 'actor_movement';

sub generate ($self)
{
	my $actor = $self->subject;
	my $movement = $actor->stats->movement;

	return {
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
		speed => $movement->speed,
		to_x => $movement->x,
		to_y => $movement->y,
	};
}

