package Game::Unit::Battle;

use Moo;
use Model::Battle;
use Types;
use List::Util qw(first);

use header;

with 'Game::Unit';

has 'battle' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Battle'],
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

