use experimental 'class';

class Resource::ActorPosition :isa(Resource);

use header;
use Utils qw(transport_floats);

use constant type => 'actor_position';
use constant is_plaintext => true;

field $actor :param(subject);    # Unit::Actor

method generate ()
{
	return [
		$actor->id,
		transport_floats($actor->variables->xy),
	];
}

