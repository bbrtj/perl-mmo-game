use experimental 'class';

class Resource::ActorEvent :isa(Resource);

use header;

use constant type => 'actor_event';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor
field $event_source :param;    # ULID
field $health_change :param;

method generate ()
{
	# affected actor id
	# current health
	# event source
	# change of actor health (may be overkill)
	return [
		$actor->id,
		$actor->variables->health,
		$event_source,
		$health_change,
	];
}

