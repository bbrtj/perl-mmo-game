use experimental 'class';

class Game::Object::Effect::Damage :isa(Game::Object::Effect);

use header;

use constant server_method => '_apply_damage_effect';

field $damage :reader :param;
field $radius :reader :param;

