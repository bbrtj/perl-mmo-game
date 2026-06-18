use experimental 'class';

class Resource::ActorState :isa(Resource);

use header;

use constant type => 'actor_state';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor

method generate ()
{
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

