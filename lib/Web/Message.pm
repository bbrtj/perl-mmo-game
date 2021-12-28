package Web::Message;

use My::Moose;
use Utils;
use Exception::WebSocket::InvalidCommand;

use header;

sub _available ($self)
{
	return state $loaded = {
		map {
			my $class = $_;
			s{^.*:}{};    # only last part
			$_ = lcfirst;    # title to camel case
			s{([A-Z]+)}{_$1};    # camel to snake case
			$_ => $class->new;
		} Utils->load_classes('Web::Message', 'Message/*.pm')
	};
}

sub get_handler ($self, $name)
{
	my $avail = $self->_available;
	Exception::WebSocket::InvalidCommand->throw
		unless exists $avail->{$name};

	return $avail->{$name};
}

sub handle ($self, $user, $data) { ... }
