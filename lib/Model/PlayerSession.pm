package Model::PlayerSession;

use My::Moose;
use Types;
use Game::Config;

use header;

extends 'Model';

with 'Model::Role::Identified';

has 'user_id' => (
	is => 'ro',
	isa => Types::Ulid,
);

__PACKAGE__->_register_cache;

