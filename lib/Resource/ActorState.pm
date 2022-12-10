package Resource::ActorState;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has option 'movement' => (
	isa => Types::InstanceOf ['Game::Object::Movement'],
);

use constant type => 'actor_state';

sub _serialize ($self)
{
	my $actor = $self->subject;

	my %result = (
		id => $actor->id,
	);

	if ($self->has_movement) {
		my $movement = $self->movement;

		%result = (
			%result,
			speed => $movement->speed,
			from_x => $actor->variables->pos_x,
			from_y => $actor->variables->pos_y,
			to_x => $movement->x,
			to_y => $movement->y,
		);
	}

	return \%result;
}

