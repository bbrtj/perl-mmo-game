use testheader;

use Utils;
use Game::Config;
use Game::Helpers;

Utils->bootstrap_lore;

my $repo = DI->get('lore_data_repo');

### test basic lore fetching
my $assassin = $repo->load_named('Game::Lore::Class', 'Assassin');
is $assassin->id, 'L.CLAS.ASSASS', 'class loaded ok';

### test global config from lore
is Game::Config->config->{zero_stats}, 8, 'global constants loaded ok';

### test helpers and translations
my $class = lore_primary_stat 'Strength';
is $class->data->translations->{pl}{name}, 'Siła', 'lore name ok';

done_testing;

