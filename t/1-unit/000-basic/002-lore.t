use testheader;

use Utils;
use DI;
use Game::Config;

Utils->bootstrap_lore;

my $repo = DI->get('lore_data');

my $assassin = $repo->load_named('Game::Lore::Class', 'Assassin');

is $assassin->id, 'L.CLAS.ASSASS', 'class loaded ok';

is Game::Config->config->{zero_stats}, 8, 'global constants loaded ok';

done_testing;

