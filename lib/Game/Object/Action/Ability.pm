package Game::Object::Action::Ability;

use My::Moose;

use header;

extends 'Game::Object::Action';

use constant server_method => 'use_ability_done';

has option 'x' => (

	# isa => Types::Num
);

has option 'y' => (

	# isa => Types::Num
);

