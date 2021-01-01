package Game::Worker::Command;

use header;
use Moo::Role;

no header;

has "_name" => (
	is => "ro",
	reader => "get_name",
	builder => "name",
	init_arg => undef,
);

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

1;
