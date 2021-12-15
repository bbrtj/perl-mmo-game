package Game::Worker::Command;

use Moo::Role;

use header;

has "_name" => (
	is => "ro",
	reader => "get_name",
	builder => "name",
	init_arg => undef,
);

# TODO: can be just a function, not a field
has "_handler" => (
	is => "ro",
	reader => "get_handler",
	default => sub ($self) { $self->can("handler") },
	init_arg => undef,
);

requires qw(
	name
	handler
);

