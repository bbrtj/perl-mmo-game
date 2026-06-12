use testheader;

use Game::Helpers;
use Game::Config;

my $repo = DI->get('lore_data_repo');

### test basic lore fetching
my $assassin = $repo->load_named('Game::Lore::Class', 'Assassin');
is $assassin->id, 'CLAS.ASSASS', 'class loaded ok';

### test global config from lore
is Game::Config->max_level, 50, 'global constants loaded ok';

### test helpers and translations
my $class = lore_primary_stat 'Strength';
is $class->translations->{pl}{name}, 'Siła', 'lore name ok';

done_testing;

