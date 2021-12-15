package Game::Unit::Battle;

use Moo;
use Game::Model::Battle;
use Types;
use List::Util qw(first);

use header;

with 'Game::Unit';

has 'battle' => (
	is => 'rw',
	isa => Types::InstanceOf ['Game::Model::Battle'],
);

has 'contestants' => (
	is => 'rw',
	isa => Types::ArrayRef [Types::InstanceOf ['Game::Unit::BattleActor']],
);

sub find_contestant ($self, $id)
{
	return first { $_->character->id eq $id }
		$self->contestants->@*;
}

