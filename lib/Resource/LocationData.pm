use experimental 'class';

class Resource::LocationData :isa(Resource);

use Resource::ActorState;

use header;

use constant type => 'location_data';

field $location :param(subject);    # Game::Lore::Location
field $actor :param;    # Unit::Actor

method generate ()
{
	return {
		id => $location->id,
		player_x => $actor->variables->pos_x,
		player_y => $actor->variables->pos_y,
	};
}

method _build_next_resources ()
{
	return [
		Resource::ActorState->new(subject => $actor),
	];
}

