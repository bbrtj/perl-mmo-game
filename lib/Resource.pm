package Resource;

use My::Moose;

use header;

has param 'subject';

around BUILDARGS => sub ($orig, $class, @args) {
	return {
		subject => shift @args,
	};
};

sub serialize ($self) { ... }

