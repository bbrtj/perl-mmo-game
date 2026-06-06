package Game::Object::Action::Ability;

use My::Moose;

use header;

extends 'Game::Object::Action';
with 'Game::Object::Role::HasPosition';

use constant server_method => 'use_ability_done';

