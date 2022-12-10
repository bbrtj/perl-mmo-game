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

has option 'stopped' => (
	isa => Types::Bool,
);

use constant type => 'actor_state';

sub generate ($self)
{
	my $actor = $self->subject;

	my %result = (
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
	);

	if ($self->has_movement) {
		my $movement = $self->movement;

		%result = (
			%result,
			speed => $movement->speed,
			to_x => $movement->x,
			to_y => $movement->y,
		);
	}
	elsif ($self->has_stopped) {
		$result{stopped} = $self->stopped;
	}

	return \%result;
}

