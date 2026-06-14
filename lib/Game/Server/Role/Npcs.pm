package Game::Server::Role::Npcs;

use My::Moose::Role;
use List::BinarySearch qw(binsearch_pos);

use all 'Game::Mechanics';
use all 'Game::Object';
use all 'Unit';
use all 'Model';

use header;

has field '_respawn_queue' => (
	isa => ArrayRef [InstanceOf ['Game::Object::Map::Spawn']],
	default => sub { [] },
);

sub _prepare_respawns ($self)
{
	my $spawns = $self->map->spawns;

	foreach my $spawn ($spawns->@*) {
		$self->_enqueue_respawn($spawn);
	}
}

sub _enqueue_respawn ($self, $spawn)
{
	my $respawns = $self->_respawn_queue;
	my $index = binsearch_pos { $a->next_respawn <=> $b->next_respawn } $spawn, $respawns->@*;

	# NOTE: this kind of splice works fast on small arrays, but gets very
	# sluggish with big arrays. This array here should never become too big.
	# See splice benchmark
	splice $respawns->@*, $index, 0, $spawn;
	return;
}

sub _process_respawns ($self)
{
	my $elapsed = server_time;
	my $queue = $self->_respawn_queue;

	while ($queue->@* > 0 && $queue->[0]->should_respawn($elapsed)) {
		my $spawn = shift $queue->@*;
		$self->_spawn_npc($spawn);
	}

	return;
}

sub _spawn_npc ($self, $spawn)
{
	my $unit = Unit::Actor->new(
		npc => Game::Object::Actor::Npc->new(
			lore => $spawn->lore,
			spawn => $spawn,
		),
		character => Model::Character->new(
			class_id => $spawn->lore->id,
			name => $spawn->lore->id,
		),
		variables => Model::CharacterVariables->new(
			location_id => $self->location->id,
			pos_x => $spawn->x,    # TODO: randomize spawn a bit
			pos_y => $spawn->y,    # TODO: randomize spawn a bit
			health => 0,
			energy => 0,
		),
	);

	$unit->variables->set_health($unit->stats->max_health);
	$unit->variables->set_energy($unit->stats->max_energy);
	$self->location->add_actor($unit);

	$self->log->debug(
		sprintf 'spawned %s at %f:%f',
		$unit->character->name,
		$unit->variables->xy,
	);
	return;
}

after 'signal_actor_died' => sub ($self, $actor) {
	return unless $actor->is_npc;

	my $spawn = $actor->npc->spawn;
	$spawn->set_next_respawn;
	$self->_enqueue_respawn($spawn);

	return;
};

after BUILD => sub ($self, @) {
	$self->_prepare_respawns;
	$self->_add_action(2 => '_process_respawns', 9);
};

