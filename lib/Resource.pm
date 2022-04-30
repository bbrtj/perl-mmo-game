package Resource;

use My::Moose;
use Types;
use Mojo::JSON qw(to_json);

use header;

has 'subject' => (
	is => 'ro',
);

around BUILDARGS => sub ($orig, $class, @args) {
	return {
		subject => shift @args,
	};
};

sub serialize ($self) { ... }

