package Exception;

use My::Moose;
use Types;

use header;

use overload
	'""' => 'stringify';

has 'msg' => (
	is => 'ro',
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
	my $msg = $self->msg // 'no message';

	return "Exception $class: $msg";
}

