use experimental 'class';

class Game::Object::Effect;

use header;

# source of the effect
field $actor :reader :param;    # Unit::Actor

# ability
field $lore :reader :param;    # Game::Lore

sub server_method ($self)
{
	...;
}

