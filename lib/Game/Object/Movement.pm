use experimental 'class';

class Game::Object::Movement;

use Game::Mechanics::Generic qw(calculate_angle_and_diagonal);

use header;

field $variables :reader :param;    # Model::CharacterVariables
field $speed :reader :param;
field $time :reader(get_time) :writer :param;
field $x :reader :param;
field $y :reader :param;
field $eta :reader;
field $angle :reader;

ADJUST
{
	($angle, my $distance) = calculate_angle_and_diagonal(
		$variables->xy,
		$x, $y
	);

	$eta = $time + $distance / $speed;
}

method xy ()
{
	return ($x, $y);
}

method finished ()
{
	return $eta == $time;
}

