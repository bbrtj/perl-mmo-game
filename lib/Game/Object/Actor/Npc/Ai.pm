package Game::Object::Actor::Npc::Ai;

use My::Moose;
use Game::Mechanics::Rng qw(rng);

use header;

has param 'parent' => (
	lax_isa => InstanceOf ['Game::Object::Actor::Npc'],
	weak_ref => 1,
);

has field 'movement_path' => (
	lax_isa => InstanceOf ['Game::TileMap::Pathfinding::Result'],
	writer => 1,
	predicate => 1,
	clearer => 1,
);

sub act ($self, $server, $actor, $elapsed = server_time)
{
	...;
}

sub move ($self, $server, $actor, $x, $y)
{
	my $path = $server->map->find_path($actor->variables->xy, $x, $y);
	return unless defined $path;

	if ($path->step_count > 0) {
		$self->set_movement_path($path);
		$self->follow_path($server, $actor);
	}
	else {
		$server->set_movement($actor, $x, $y);
	}

	return;
}

sub follow_path ($self, $server, $actor)
{
	my $path = $self->movement_path;

	my ($x, $y) = $path->next_step;

	if (defined $x) {
		$x += 0.5 + ((1 - rng) * 0.3) * (rng() <=> 0.5);
		$y += 0.5 + ((1 - rng) * 0.3) * (rng() <=> 0.5);
	}
	else {
		$self->clear_movement_path;
		return;
	}

	$server->set_movement($actor, $x, $y);
	return;
}

