use experimental 'class';

class Resource::ActorPosition :isa(Resource);

use header;

use constant type => 'actor_position';

field $actor :param(subject);    # Unit::Actor

method generate ()
{
	return {
		id => $actor->id,
		x => $actor->variables->pos_x,
		y => $actor->variables->pos_y,
	};
}

