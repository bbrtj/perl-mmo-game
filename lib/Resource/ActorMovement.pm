use experimental 'class';

class Resource::ActorMovement :isa(Resource);

use header;

use constant type => 'actor_movement';

field $actor :param(subject);    # Unit::Actor

method generate ()
{
	my $movement = $actor->stats->movement;

	return {
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
		speed => $movement->speed,
		to_x => $movement->x,
		to_y => $movement->y,
	};
}

