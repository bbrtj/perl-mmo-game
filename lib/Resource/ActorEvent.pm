package Resource::ActorEvent;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has param 'event_source' => (
	isa => Types::ULID,
);

has param 'health_change' => (
	isa => Types::Num,
);

use constant type => 'actor_event';
use constant is_plaintext => true;

sub generate ($self)
{
	my $actor = $self->subject;

	# affected actor id
	# current health
	# event source
	# change of actor health (may be overkill)
	return [
		$actor->id,
		$actor->variables->health,
		$self->event_source,
		$self->health_change,
	];
}

