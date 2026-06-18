use experimental 'class';

class Game::Object::Action;

use header;

field $actor :reader :param;    # Unit::Actor
field $lore :reader :param;    # Game::Lore
field $duration :reader :param;
field $start_time = time;
field $eta :reader;
field $cancelled :reader :writer = false;

sub server_method ($self)
{
	...;
}

ADJUST
{
	$eta = $start_time + $duration;
}

method finished ($time = server_time)
{
	return $time >= $eta || $cancelled;
}

