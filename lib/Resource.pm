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
	if (!defined $serialized) {
		my $gen = $self->generate;
		if ($self->is_plaintext) {
			croak "Bad resource data type generated for " . ref $self
				unless ref $gen eq 'ARRAY';

			$serialized = join Server::Config->PROTOCOL_SEPARATOR, $gen->@*;
		}
		else {
			$serialized = __serialize $gen;
		}
	}

	return $serialized;
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

