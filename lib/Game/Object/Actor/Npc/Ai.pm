package Game::Object::Actor::Npc::Ai;

use My::Moose;

use header;

has param 'parent' => (
	lax_isa => InstanceOf ['Game::Object::Actor::Npc'],
	weak_ref => 1,
);

sub act ($self, $server, $actor, $elapsed = server_time)
{
	...;
}

