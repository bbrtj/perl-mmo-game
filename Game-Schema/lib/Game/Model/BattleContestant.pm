package Game::Model::BattleContestant;

use header;
use Moose;
use Game::Types qw(Uuid Num PositiveInt);

no header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'battle_id' => (
	is => 'ro',
	isa => Uuid,
	required => 1,
);

has 'character_id' => (
	is => 'ro',
	isa => Uuid,
	required => 1,
);

has 'pos_x' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'pos_y' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'initiative' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

__PACKAGE__->_register;
