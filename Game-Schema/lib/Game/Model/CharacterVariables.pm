package Game::Model::CharacterVariables;

use header;
use Moose;
use Game::Types qw(LoreId PositiveInt Num);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'experience' => (
	is => 'ro',
	isa => PositiveInt,
	required => 0,
	default => sub { 0 },
);

has 'location' => (
	is => 'ro',
	isa => LoreId,
	required => 1,
);

has 'health' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'mana' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

__PACKAGE__->_register;
