package Exception;

use My::Moose;
use Types;

use header;

# TODO: stringify with the msg if present?
has 'msg' => (
	is => 'ro',
	isa => Types::Str,
);

sub throw ($self, @args)
{
	die $self if ref $self;
	die $self->new(@args);
}
