use experimental 'class';

class Resource::ActorEventHealth :isa(Resource);

use header;
use Utils qw(transport_floats);

use constant type => 'actor_event_health';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor
field $source :param;    # ULID
field $lore :param;    # Lore string
field $change :param;    # float

method generate ()
{
	# affected actor id
	# current health
	# event source
	# change of actor health (may be overkill)
	return [
		$actor->id,
		$source,
		$lore,
		transport_floats(
			$actor->variables->health,
			$change,
		),
	];
}

