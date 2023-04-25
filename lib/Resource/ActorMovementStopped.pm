package Resource::ActorPosition;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

use constant type => 'actor_position';

sub generate ($self)
{
	my $actor = $self->subject;

	return {
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
	};
}

