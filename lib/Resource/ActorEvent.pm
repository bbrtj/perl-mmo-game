package Resource::ActorEvent;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has option 'event_source' => (
	isa => Types::ULID,
);

has option 'health_change' => (
	isa => Types::Num,
);

use constant type => 'actor_event';
use constant is_plaintext => !!1;

sub generate ($self)
{
	my $actor = $self->subject;

	# actor id
	# health, max health
	# energy, max energy
	# optional: event source, change of actor health
	return [
		$actor->id,
		$actor->variables->health,
		$actor->stats->max_health,
		$actor->variables->energy,
		$actor->stats->max_energy,
		$self->event_source // '',
		$self->health_change // '',
	];
}

