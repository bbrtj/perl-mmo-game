use experimental 'class';

class Game::Object::Action::Ability :isa(Game::Object::Action);

use header;

use constant server_method => 'use_ability_done';

field $x :reader :param;
field $y :reader :param;

method xy()
{
	return ($x, $y);
}

