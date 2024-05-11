package Resource;

use My::Moose;

use Server::Config;

use header;

has param 'subject';

has field 'serialized' => (
	lazy => 1,
);

has field 'next_resources' => (
	lazy => 1,
);

sub type { ... }

sub is_plaintext
{
	return !!0;
}

sub generate ($self) { ... }

sub _build_serialized ($self)
{
	my $gen = $self->generate;
	if ($self->is_plaintext) {
		$gen = join Server::Config->PROTOCOL_SEPARATOR, $gen->@*
			if is_arrayref $gen;
	}
	else {
		$gen = __serialize $gen;
	}

	return $gen;
}

sub _build_next_resources
{
	return [];
}

