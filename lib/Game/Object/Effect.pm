package Game::Object::Effect;

use My::Moose;
use Game::Mechanics::Generic;

use header;

# source of the effect

has param 'actor' => (
	lax_isa => InstanceOf ['Unit::Actor'],
);

# ability

has param 'lore' => (
	lax_isa => InstanceOf ['Game::Lore'],
);

sub server_method ($self)
{
	...;
}

