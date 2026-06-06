package Game::Object::Effect::Damage;

use My::Moose;

use header;

use constant server_method => '_apply_damage_effect';

extends 'Game::Object::Effect';

has param 'damage' => (
	lax_isa => Num,
);

has param 'radius' => (
	lax_isa => PositiveNum,
);

