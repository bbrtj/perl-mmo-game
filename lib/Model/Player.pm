package Model::Player;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'user_id' => (
	isa => ULID,
	traits => [qw(Stored)],
);

has param 'online' => (
	isa => Bool,
	default => false,
	traits => [qw(Stored)],
);

has param 'last_online' => (
	coerce => Maybe [DateTime],
	default => undef,
	traits => [qw(Stored)],
);

has param 'created_at' => (
	coerce => DateTime,
	default => sub { int time },
	traits => [qw(Stored)],
);

sub set_offline ($self)
{
	$self->set_online(false);
	$self->set_last_online(int time);

	return;
}

__PACKAGE__->_register;

