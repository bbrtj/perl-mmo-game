package Model::PlayerSession;

use My::Moose;
use Types;
use Game::Config;

use header;

use constant {
	STATE_NEW => 0,
	STATE_LOGGED_IN => 1,
	STATE_PLAYING => 2,
};

extends 'Model';

with 'Model::Role::Identified';

has 'user_id' => (
	is => 'ro',
	isa => Types::Ulid,
);

has 'state' => (
	is => 'ro',
	isa => Types::Enum [STATE_NEW, STATE_LOGGED_IN, STATE_PLAYING],
	default => sub { STATE_NEW },
);

# in-game location
has 'location' => (
	is => 'ro',
	isa => Types::LoreId,
);

__PACKAGE__->_register_cache;

