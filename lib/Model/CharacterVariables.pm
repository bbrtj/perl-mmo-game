package Model::CharacterVariables;

use My::Moose;
use Types;

use header;

extends 'Model';

with 'Model::Role::Stored';

has 'experience' => (
	is => 'ro',
	isa => Types::PositiveOrZeroInt,
	required => 0,
	default => sub { 0 },
);

has 'location' => (
	is => 'ro',
	isa => Types::LoreId,
	required => 1,
);

has 'health' => (
	is => 'ro',
	isa => Types::Num,
	required => 1,
);

has 'mana' => (
	is => 'ro',
	isa => Types::Num,
	required => 1,
);

__PACKAGE__->_register;
