BEGIN {
	$ENV{TEST_NO_REGENERATION} = 1;
}

use Server::TestProcess;
use ActorTest;
use Game::Object::Map::Spawn;
use testheader;

my $process = Server::TestProcess->new;
$process->server->enqueue_respawn(
	Game::Object::Map::Spawn->new(
		lore => DI->get('lore_data_repo')->load('npc.bograt'),
		x => 1,
		y => 1,
	)
);

is scalar keys $process->location->actors->%*, 0, 'npc not spawned yet ok';
$process->ticks(50);
is scalar keys $process->location->actors->%*, 1, 'npc spawned ok';

my ($npc_id) = keys $process->location->actors->%*;
my ($actor1) = ActorTest->create_actor(
	character_params => {
		alliance_id => 'alli.colon',
	},
	variables_params => {
		health => 10,
		pos_x => 8,
		pos_y => 1,
	}
);

# add actor to the game world
$process->location->add_actor($actor1);
$process->ticks(50);
is $process->location->actors->{$npc_id}->npc->aggro_map, {}, 'npc not aggroed yet ok';

# move the actor closer to the npc
$process->server->set_movement($actor1, 6, 1);
$process->ticks(50);
is $process->location->actors->{$npc_id}->npc->aggro_map, {$actor1->id => D()}, 'npc aggroed ok';

# let the npc kill the actor (may take many ticks, since npc needs to get to
# the actor and hit him a couple of times)
$process->ticks(600);
ok $actor1->variables->dead, 'actor died ok';
is $process->location->actors->{$npc_id}->npc->aggro_map, {}, 'actor removed from npc aggro map';

done_testing;

