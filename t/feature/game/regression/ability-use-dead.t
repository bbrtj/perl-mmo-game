BEGIN {
	$ENV{TEST_NO_REGENERATION} = 1;
}

use Server::TestProcess;
use ActorTest;
use testheader;

use constant STARTING_HEALTH => 10;

my $process = Server::TestProcess->new;

my ($actor1) = ActorTest->create_actor(
	character_params => {
		alliance_id => 'alli.colon',
	},
	variables_params => {
		health => STARTING_HEALTH,
		pos_x => 8,
		pos_y => 2,
	}
);
$process->location->add_actor($actor1);

my ($actor2) = ActorTest->create_actor(
	character_params => {
		alliance_id => 'alli.rem',
	},
	variables_params => {
		health => 1,
		pos_x => 9,
		pos_y => 2,
	}
);
$process->location->add_actor($actor2);

# face each other
$process->server->set_movement($actor1, 8.3, 2);
$process->server->set_movement($actor2, 8.7, 2);

# hit each other
my $ability = DI->get('lore_data_repo')->load('abil.strike');

# actor 1 hits first
$process->server->use_ability($actor1, $ability, 0, 0);
$process->ticks(5);

# actor 2 tries to hit, but gets hit and killed
$process->server->use_ability($actor2, $ability, 0, 0);
$process->ticks(100);

# was it what happened?
ok $actor2->variables->dead, 'actor 2 died ok';
is $actor1->variables->health, STARTING_HEALTH, 'actor 1 health unchanged ok';

done_testing;

