package Unit::Actor;

use My::Moose;
use all 'Model';

use header;

extends 'Unit';

has option 'player' => (
	isa => Types::Maybe [Types::InstanceOf ['Model::Player']],
	predicate => 'is_player',
);

has option 'npc' => (

	# isa => Types::Maybe[Types::InstanceOf['Model::Npc']],
	predicate => 'is_npc',
);

has param 'character' => (
	isa => Types::InstanceOf ['Model::Character'],
);

has param 'variables' => (
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

