package Game::Model::CharacterCache;

use header;
use Moose;
use Game::Types qw(PositiveInt Num NonEmptySimpleStr);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'level' => (
	is => 'ro',
	isa => PositiveInt,
);

has 'health_max' => (
	is => 'ro',
	isa => PositiveInt,
);

has 'health_regen' => (
	is => 'ro',
	isa => Num,
);

has 'mana_max' => (
	is => 'ro',
	isa => PositiveInt,
);

has 'mana_regen' => (
	is => 'ro',
	isa => Num,
);

has 'stats' => (
	is => 'ro',
	isa => NonEmptySimpleStr,
	required => 1,
);

__PACKAGE__->_install_writers;
