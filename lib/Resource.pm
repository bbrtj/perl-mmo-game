package Resource;

use My::Moose;
use Types;

use header;

has 'subject' => (
	is => 'ro',
	isa => Types::InstanceOf['Unit'],
);

has 'more_data' => (
	is => 'ro',
	default => sub { {} },
);

around BUILDARGS => sub ($orig, $class, @args) {
	my $subject = shift @args;

	return {
		subject => $subject,
		more_data => { @args },
	};
};

sub hash ($self) { ... }
