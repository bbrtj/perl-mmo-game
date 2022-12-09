package Resource;

use My::Moose;

use header;

has param 'subject';

has field 'serialized' => (
	lazy => '_serialize'
);

sub _serialize ($self) { ... }

