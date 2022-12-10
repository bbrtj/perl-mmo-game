package Resource::Discovery;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::HashRef [
		Types::ArrayRef [Types::ULID]
	],

	default => sub { {} },

	'handles{}' => {
		'_add' => 'set',
	},
);

use constant type => 'discovery';

sub new_actors ($self, $list)
{
	return $self->_add('+actors', $list);
}

sub old_actors ($self, $list)
{
	return $self->_add('-actors', $list);
}

sub generate ($self)
{
	return $self->subject;
}

