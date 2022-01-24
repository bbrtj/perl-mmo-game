package Unit::Actor;

use My::Moose;
use DI;
use Model::Player;
use Model::Character;
use Model::CharacterVariables;
use Types;

use header;

extends 'Unit';

has 'player' => (
	is => 'rw',
	isa => Types::Maybe [Types::InstanceOf ['Model::Player']],
	predicate => 'is_player',
);

has 'npc' => (
	is => 'rw',

	# isa => Types::Maybe[Types::InstanceOf['Model::Npc']],
	predicate => 'is_npc',
);

has 'character' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::Character'],
);

has 'variables' => (
	is => 'rw',
	isa => Types::InstanceOf ['Model::CharacterVariables'],
);

sub models ($self)
{
	# we do not return npc / player here on purpose
	return [
		$self->character,
		$self->variables,
	];
}

