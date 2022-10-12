package Exception;

use My::Moose;

use header;

use overload
	'""' => 'stringify',
	bool => sub { 1 },
	fallback => 1;

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
	my $msg = $self->msg ? ': ' . $self->msg : '';

	return "Exception $class$msg";
}

