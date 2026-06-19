use experimental 'class';

class Game::Object::Projectile;

use Game::Mechanics::Generic qw(find_frontal_point);

use header;

field $id :reader = Types::ULID::ulid;
field $effect :reader :param;    # Game::Object::Effect
field $speed :reader :param;
field $angle :reader :param;
field $time :reader(get_time) :writer :param = time;
field $max_distance :reader :param;
field $eta :reader;
field @discovered_by :reader;

field $x :reader :writer;
field $y :reader :writer;

ADJUST
{
	$eta = $time + $max_distance / $speed;
	my $actor = $effect->actor;
	($x, $y) = find_frontal_point($actor->variables->xy, $angle, $actor->stats->size);
}

method actor ()
{
	return $effect->actor;
}

method xy ()
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

