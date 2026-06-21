use v5.42;
use experimental 'class';

class X;

use overload
	'""' => 'stringify',
	bool => sub { true },
	fallback => 1;

field $msg :reader :param = undef;

ADJUST {
	$msg //= $self->_build_msg;
}

method _build_msg ()
{
	return undef;
}

sub raise ($self, $msg = undef, %args)
{
	die $self if ref $self;
	die $self->new(msg => $msg, %args);
}

method stringify (@)
{
	my $class = ref $self;
	my $msg_text = defined $msg ? ": $msg" : '';

	return "Exception $class$msg_text";
}

