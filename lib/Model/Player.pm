package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'user_id' => (
	isa => Types::ULID,
);

has param 'online' => (
	isa => Types::Bool,
	default => !!0,
);

has param 'last_online' => (
	coerce => Types::Maybe [Types::DateTime],
	default => undef,
);

has param 'created_at' => (
	coerce => Types::DateTime,
	default => sub { time },
);

sub set_offline ($self)
{
	$self->set_online(!!0);
	$self->set_last_online(time);

	return;
}

__PACKAGE__->_register;

