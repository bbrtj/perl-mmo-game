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

has 'language' => (
	is => 'ro',
	isa => Types::Enum[Game::Config->config->{supported_langs}->@*],
);

__PACKAGE__->_register_cache;

