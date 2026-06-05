package Game::Object::Action::Ability;

use My::Moose;

use header;

extends 'Game::Object::Action';

use constant server_method => 'use_ability_done';

has option 'x' => (
	lax_isa => Num,
);

has option 'y' => (
	lax_isa => Num,
);

