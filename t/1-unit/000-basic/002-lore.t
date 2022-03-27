use testheader;

use Utils;
use Game::Lore;
use Game::Config;

Utils->bootstrap_lore;

my $assassin = Game::Lore->get_named('Game::Lore::Class', 'Assassin');

is $assassin->id, 'L.CLAS.ASSASS', 'class loaded ok';

is Game::Config->config->{zero_stats}, 8, 'global constants loaded ok';

done_testing;

