package Model::BattleContestant;

use Moose;
use Types;

use header;

with 'Model', 'Model::Role::Stored';

has 'battle_id' => (
	is => 'ro',
	isa => Types::Ulid,
	required => 1,
);

has 'character_id' => (
	is => 'ro',
	isa => Types::Ulid,
	required => 1,
);

has 'pos_x' => (
	is => 'ro',
	isa => Types::Num,
	required => 1,
);

has 'pos_y' => (
	is => 'ro',
	isa => Types::Num,
	required => 1,
);

has 'initiative' => (
	is => 'ro',
	isa => Types::PositiveInt,
	required => 1,
);

has 'team' => (
	is => 'ro',
	isa => Types::PositiveOrZeroInt,
	required => 1,
);

__PACKAGE__->_register;
