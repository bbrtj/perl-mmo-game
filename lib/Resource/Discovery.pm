package Resource::Discovery;

use My::Moose;

use header;

extends 'Resource';

# TODO: Sub::HandlesVia bug
has field '_subject' => (
	isa => Types::HashRef [
		Types::ArrayRef [Types::Ulid]
	],

	default => sub { {} },

	'handles{}' => {
		'_add' => 'store',
	},
);

sub new_players ($self, @list)
{
	return $self->_add('+players', \@list);
}

sub old_players ($self, @list)
{
	return $self->_add('-players', \@list);
}

sub serialize ($self)
{
	return {discovery => $self->_subject};
}

