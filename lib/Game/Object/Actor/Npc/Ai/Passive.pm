package Game::Object::Actor::Npc::Ai::Passive;

use My::Moose;
use Game::Mechanics::Distance;

use header;

extends 'Game::Object::Actor::Npc::Ai';

has param 'social_aggro_range' => (
	lax_isa => PositiveNum,
	default => sub { 3 },
);

has param 'social_aggro_type' => (
	lax_isa => Str,
	lazy => sub ($self) { $self->parent->race->name },
);

with qw(
	Game::Object::Actor::Npc::Ai::Role::Wandering
	Game::Object::Actor::Npc::Ai::Role::CanFight
);

sub act ($self, $server, $npc_actor, $elapsed = server_time)
{
	$self->fight($server, $npc_actor)
		or $self->wander($server, $npc_actor, $elapsed);

	# TODO: social aggro

	return;
}

