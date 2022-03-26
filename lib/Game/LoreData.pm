package Game::LoreData;

use My::Moose;
use Types;

use header;

has 'translations' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
);

has 'define' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
);

has 'uses' => (
	is => 'ro',
	isa => Types::ArrayRef,
	default => sub { [] },
);

