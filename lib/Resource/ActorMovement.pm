use experimental 'class';

class Resource::ActorMovement :isa(Resource);

use header;
use Utils qw(transport_floats);

use constant type => 'actor_movement';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor

method generate ()
{
	my $movement = $actor->stats->movement;

	return [
		$actor->id,
		transport_floats(
			$actor->variables->xy,
			$movement->speed,
			$movement->xy,
		),
	];
}

