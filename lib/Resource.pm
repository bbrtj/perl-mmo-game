use experimental 'class';

class Resource;

use Server::Config;

use header;

field $serialized;
field $next_resources;

use constant type => undef;
use constant is_plaintext => false;

method generate () { ... }

method serialized ()
{
	return $serialized //= $self->serialize($self->generate);
}

method serialize ($data)
{
	return join Server::Config->PROTOCOL_SEPARATOR, $data->@*
		if $self->is_plaintext;
	return __serialize $data;
}

method deserialize ($data)
{
	return [split quotemeta Server::Config->PROTOCOL_SEPARATOR, $data]
		if $self->is_plaintext;
	return __deserialize $data;
}

method next_resources ()
{
	if (!defined $next_resources) {
		$next_resources = $self->_build_next_resources;
	}

	return $next_resources;
}

method _build_next_resources ()
{
	return [];
}

