package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'user_id' => (
	isa => ULID,
);

has param 'online' => (
	isa => Bool,
	default => false,
);

has param 'last_online' => (
	coerce => Maybe [DateTime],
	default => undef,
);

has param 'created_at' => (
	coerce => DateTime,
	default => sub { int time },
);

sub set_offline ($self)
{
	$self->set_online(false);
	$self->set_last_online(int time);

	return;
}

__PACKAGE__->_register;

