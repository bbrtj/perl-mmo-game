package Resource::ActorEvent;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['Unit::Actor'],
);

has param 'event_source' => (
	isa => ULID,
);

has param 'health_change' => (
	isa => Num,
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

