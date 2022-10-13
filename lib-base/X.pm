package X;

use v5.36;
use My::Moose;

use overload
	'""' => 'stringify',
	bool => sub { 1 },
	fallback => 1;

has option 'msg' => (
	isa => Types::Str,
);

sub throw ($self, @args)
{
	die $self if ref $self;
	die $self->new(@args);
}

sub stringify ($self, @)
{
	my $class = ref $self;
	my $msg = $self->has_msg ? ': ' . $self->msg : '';

	return "Exception $class$msg";
}

1;

