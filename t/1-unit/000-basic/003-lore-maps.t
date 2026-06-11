use testheader;

use Game::Helpers;
use Game::Config;

my $repo = DI->get('lore_data_repo');

### test basic lore fetching
my $harbor = $repo->load_named('Game::Lore::Location', 'Cape Peril - Harbor');
is $harbor->id, 'LOC.CP_HARBOR', 'location loaded ok';

is $harbor->map->size_x, 40, 'size x ok';
is $harbor->map->size_y, 40, 'size y ok';

isa_ok $harbor->map->coordinates->[0][0], 'Game::TileMap::Tile';

done_testing;

