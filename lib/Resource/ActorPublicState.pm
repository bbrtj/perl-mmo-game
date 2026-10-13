use experimental 'class';

class Resource::ActorPublicState :isa(Resource);

use header;
use Utils qw(transport_floats);

use constant type => 'actor_public_state';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor

method generate ()
{
	my $stats = $actor->stats;

	# actor id
	# level
	# health, max health, health_regeneration
	# energy, max energy, energy_regeneration
	return [
		$actor->id,
		$stats->level,
		transport_floats(
			$actor->variables->health,
			$stats->max_health,
			$stats->health_regeneration,
			$actor->variables->energy,
			$stats->max_energy,
			$stats->energy_regeneration,
			$stats->size,
		),
	];
}

