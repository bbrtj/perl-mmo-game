package X;

use v5.36;
use My::Moose;
use Types::Standard;

use overload
	'""' => 'stringify',
	bool => sub { 1 },
	fallback => 1;

has option 'msg' => (
	isa => Types::Standard::Str,
);

sub throw ($self, $msg = undef, %args)
{
	die $self if ref $self;

	$args{msg} = $msg if $msg;
	die $self->new(%args);
}

sub stringify ($self, @)
{
	my $class = ref $self;
	my $msg = $self->has_msg ? ': ' . $self->msg : '';

	return "Exception $class$msg";
}

1;

