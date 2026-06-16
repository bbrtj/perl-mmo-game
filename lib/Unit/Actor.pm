package Unit::Actor;

use My::Moose;
use all 'Model';
use Game::Object::Actor::Npc;
use Game::RNG qw(random_number);
use List::Util qw(min);

use header;

extends 'Unit';

has option 'player' => (
	isa => Maybe [InstanceOf ['Model::Player']],
	predicate => 'is_player',
);

has option 'npc' => (
	lax_isa => Maybe [InstanceOf ['Game::Object::Actor::Npc']],
	predicate => 'is_npc',
);

has param 'character' => (
	isa => InstanceOf ['Model::Character'],
);

has param 'variables' => (
	isa => InstanceOf ['Model::CharacterVariables'],
);

has field 'stats' => (
	constructed => ['Game::Object::Actor::Stats', sub { shift->new(parent => $_[0]) }],
);

sub BUILD ($self, $args)
{
	$self->_set_id($self->player->id)
		if $self->is_player;

	return;
}

sub models ($self)
{
	return [
		$self->variables,
	];
}

# returns bigger numbers with higher luck
sub rng ($self)
{
	return min(1, random_number(0, $self->stats->luck_effect));
}

__END__

=pod

Actor is an unit that is used in all mechanics checks and so on. Because of
that, we do not intend to save player or character data, as it should not be
changed in this unit. Use other units or plain models for that instead.

