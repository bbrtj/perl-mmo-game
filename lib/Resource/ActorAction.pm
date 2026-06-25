use experimental 'class';

class Resource::ActorAction :isa(Resource);

use header;
use Utils qw(transport_float);

use constant type => 'actor_action';
use constant is_plaintext => true;

field $action :param(subject);    # Game::Object::Action

method generate ()
{
	my $duration = $action->cancelled ? 0 : $action->duration;

	# actor id
	# action name
	# action duration (0 if cancelled)
	return [
		$action->actor->id,
		$action->lore->id,
		transport_float $duration,
	];
}

