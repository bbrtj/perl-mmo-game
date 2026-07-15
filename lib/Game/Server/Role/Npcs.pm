package Game::Server::Role::Npcs;

use My::Moose::Role;
use My::PQ::Elem;
use My::PQ;

use Game::Helpers;
use Game::Mechanics::Character::Statistics qw(get_exp_for_level);

use all 'Game::Object';
use all 'Unit';
use all 'Model';

use header;

requires qw(
	location
	get_discovered_by
);

has field '_respawn_queue' => (
	default => sub { My::PQ->new },
);

sub _prepare_respawns ($self)
{
	my $spawns = $self->map->spawns;

	foreach my $spawn ($spawns->@*) {
		$self->enqueue_respawn($spawn);
	}
}

sub enqueue_respawn ($self, $spawn)
{
	$self->_respawn_queue->add(My::PQ::Elem->new(val => $spawn, cmp_val => $spawn->next_respawn));
	return;
}

sub _process_respawns ($self)
{
	my $elapsed = server_time;
	my $queue = $self->_respawn_queue;
	my $el;

	while (($el = $queue->top) && $el->val->should_respawn($elapsed)) {
		$queue->extract_top;
		$self->_spawn_npc($el->val);
	}

	return;
}

sub _process_ai ($self)
{
	my $elapsed = server_time;

	foreach my $actor ($self->location->get_npcs->@*) {
		next unless my $ai = $actor->npc->ai;

		# TODO: for now, we reduce aggro of all npcs, even undiscovered ones
		$actor->npc->reduce_aggro;

		next unless $self->is_discovered($actor->id);
		$ai->act($self, $actor, $elapsed);
	}
}

sub _spawn_npc ($self, $spawn)
{
	state $remnants_lore = lore_alliance 'Remnants';
	my $npc_object = Game::Object::Actor::Npc->new(
		spawn => $spawn,
	);

	my $unit = Unit::Actor->new(
		npc => $npc_object,
		character => Model::Character->new(
			class_id => $spawn->lore->id,
			race_id => $npc_object->race->id,
			alliance_id => $remnants_lore->id,
			name => $spawn->lore->id,
		),
		variables => Model::CharacterVariables->new(
			location_id => $self->location->id,
			pos_x => $spawn->x,    # TODO: randomize spawn a bit
			pos_y => $spawn->y,    # TODO: randomize spawn a bit
			experience => get_exp_for_level($spawn->lore->level),
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

sub _plan_respawn ($self, $actor)
{
	my $spawn = $actor->npc->spawn;
	$spawn->set_next_respawn;
	$self->enqueue_respawn($spawn);

	return;
}

sub _proceed_ai_movement ($self, $actor)
{
	return unless my $ai = $actor->npc->ai;
	return unless $ai->has_movement_path;

	$ai->follow_path($self, $actor);
	return;
}

after BUILD => sub ($self, @) {
	$self->_prepare_respawns;

	$self->_add_action(2 => '_process_respawns', 9);
	$self->_add_action(1 => '_process_ai');

	$self->_add_signal(actor_died => '_plan_respawn', '$actor->is_npc');
	$self->_add_signal(movement_ended => '_proceed_ai_movement', '$actor->is_npc');
};

