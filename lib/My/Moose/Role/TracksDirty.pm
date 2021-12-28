package My::Moose::Role::TracksDirty;

use v5.32;
use warnings;
use Moose::Role;

has '_dirty' => (
	is => 'ro',
	isa => 'HashRef',
	default => sub { {} },
	lazy => 1,
	clearer => '_clear_dirty',
);

1;
