package Resource;

use My::Moose;
use Types;
use Mojo::JSON qw(to_json);

use header;

has 'subject' => (
	is => 'ro',
	isa => Types::InstanceOf ['Unit'],
);

around BUILDARGS => sub ($orig, $class, @args) {
	return {
		subject => shift @args,
	};
};

sub hash ($self) { ... }

