package Resource;

use My::Moose;

use header;

has param 'subject';

has field 'serialized' => (
	lazy => sub { $_[0]->generate },
);

sub type { ... }

sub generate ($self) { ... }

