BEGIN {
	$ENV{TEST_NO_REGENERATION} = 1;
	$ENV{TEST_NO_RANDOM} = 1;
}

use Server::TestProcess;
use ActorTest;
use Game::Object::Map::Spawn;
use Game::Mechanics::Character::Statistics qw(get_exp_for_level);
use List::Util qw(first);
use testheader;

my $process = Server::TestProcess->new;
$process->server->enqueue_respawn(
	Game::Object::Map::Spawn->new(
		lore => DI->get('lore_data_repo')->load('npc.rat'),
		x => 1,
		y => 1,
	)
);

is scalar keys $process->location->actors->%*, 0, 'npc not spawned yet ok';
$process->ticks(50);
is scalar keys $process->location->actors->%*, 1, 'npc spawned ok';

my ($npc_actor) = values $process->location->actors->%*;

my ($actor1) = ActorTest->create_actor(
	character_params => {
		alliance_id => 'alli.colon',
	},
	variables_params => {
		experience => get_exp_for_level(2) - 1,
		health => 10,
		pos_x => 0.7,
		pos_y => 0.7,
	}
);

my $starting_experience = $actor1->variables->experience;

# add actor to the game world, change health of npc
$process->location->add_actor($actor1);
$npc_actor->variables->set_health(1);

# IMPORTANT: wait for discovery to discover the npc for the player. Without
# this, the npc is not added to the combat quad tree.
$process->ticks(50);

# clear sent_data from process, so that we can examine it later
$process->sent_data->@* = ();

# move the actor closer to the npc and use the ability
$process->server->set_movement($actor1, 0.8, 0.8);
$process->server->use_ability($actor1, $process->server->lore_data_repo->load('abil.strike'), 0, 0);
$process->ticks(50);

# npc should be killed, check if experience was granted
# TODO: check with another player trying to steal the kill (but not having the tag)
ok $npc_actor->variables->dead, 'npc dead ok';
is $actor1->variables->experience, number_gt($starting_experience), 'experience granted ok';

# player should level up. Check if we had a message with increased level number
my $found = first {
	$_->[0] eq $actor1->id
		&& $_->[1] isa 'Resource::ActorPublicState'
		&& $_->[1]->generate->[0] eq $actor1->id
} reverse $process->sent_data->@*;

ok $found, 'state received ok';

done_testing;

