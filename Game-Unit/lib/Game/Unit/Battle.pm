package Game::Unit::Battle;

use header;
use Moo;
use Game::Model::Battle;
use Game::Types qw(InstanceOf ArrayRef);
use List::Util qw(first);

no header;

with 'Game::Unit';

has 'battle' => (
	is => 'rw',
	isa => InstanceOf ['Game::Model::Battle'],
);

has 'contestants' => (
	is => 'rw',
	isa => ArrayRef [InstanceOf ['Game::Unit::BattleActor']],
);

sub find_contestant ($self, $id)
{
	return first { $_->character->id eq $id }
		$self->contestants->@*;
}

1;

