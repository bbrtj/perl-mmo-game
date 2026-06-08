package Resource::ActorState;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['Unit::Actor'],
);

use constant type => 'actor_state';
use constant is_plaintext => true;

sub generate ($self)
{
	my $actor = $self->subject;
	my $stats = $actor->stats;

	# actor id
	# health, max health, health_regeneration
	# energy, max energy, energy_regeneration
	return [
		$actor->id,
		$actor->variables->health,
		$stats->max_health,
		$stats->health_regeneration,
		$actor->variables->energy,
		$stats->max_energy,
		$stats->energy_regeneration,
		$stats->size,
	];
}

