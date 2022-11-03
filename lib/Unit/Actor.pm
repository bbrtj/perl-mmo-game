package Unit::Actor;

use My::Moose;
use Model;

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
	return [
		$self->variables,
	];
}

__END__

=pod

Actor is an unit that is used in all mechanics checks and so on. Because of
that, we do not intend to save player or character data, as it should not be
changed in this unit. Use other units or plain models for that instead.

