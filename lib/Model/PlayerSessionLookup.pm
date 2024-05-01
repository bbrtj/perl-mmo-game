package Model::PlayerSessionLookup;

use My::Moose;

use header;

extends 'Model';
with 'Model::Role::Identified';

has extended 'id' => (
	required => 1,
	isa => Types::ShortStr,
);

has param 'session_id' => (
	isa => Types::ULID,
);

sub dummy { die 'No dummy for ' . __PACKAGE__ }

__PACKAGE__->_register_cache;

