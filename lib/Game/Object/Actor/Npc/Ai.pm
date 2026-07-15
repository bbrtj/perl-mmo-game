package Game::Object::Actor::Npc::Ai;

use My::Moose;
use Game::Mechanics::Generic qw(calculate_angle find_frontal_point);

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

has field 'movement_target' => (
	lax_isa => Tuple [Num, Num],
	writer => 1,
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
		$self->set_movement_target([$x, $y]);
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

	if (!defined $x) {
		$self->clear_movement_path;
		return;
	}

	$x += 0.5;
	$y += 0.5;
	my $angle = calculate_angle($x, $y, $self->movement_target->@*);
	$server->set_movement($actor, find_frontal_point($x, $y, $angle, 0.5 - $actor->stats->size));
	return;
}

