use experimental 'class';

class Game::Object::Projectile;

use Game::Object::Effect;

use header;

field $id :reader = Types::ULID::ulid;
field $actor :reader :param;    # Unit::Actor
field $effect :reader :param;    # Game::Object::Effect
field $speed :reader :param;
field $angle :reader :param;
field $time :reader(get_time) :writer :param = time;
field $max_distance :reader :param;
field $eta :reader;
field @discovered_by :reader;

field $x :reader :writer :param;
field $y :reader :writer :param;

ADJUST
{
	$eta = $time + $max_distance / $speed;
}

method xy()
{
	return ($x, $y);
}

method set_discovered_by (@values)
{
	@discovered_by = @values;
}

method finished ()
{
	return $time >= $eta;
}

method set_finished ()
{
	$eta = 0;
	return;
}

