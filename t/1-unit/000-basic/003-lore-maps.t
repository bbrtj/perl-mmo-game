use testheader;

use Utils;
use DI;
use Game::Config;
use Game::Helpers;

Utils->bootstrap_lore;

my $repo = DI->get('lore_data');

### test basic lore fetching
my $harbor = $repo->load_named('Game::Lore::Location', 'Cape Peril - Harbor');
is $harbor->id, 'L.LOC.CP_HARBOR', 'location loaded ok';

is $harbor->data->map->size_x, 10, 'size x ok';
is $harbor->data->map->size_y, 10, 'size y ok';

is $harbor->data->map->coordinates, [
	[0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
	[0, 1, 1, 1, 1, 1, 0, 0, 0, 0],
	[0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
	[0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
	[0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
	[0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
	[0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
	[0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
	[0, 1, 1, 1, 1, 1, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
], 'map loaded ok';

done_testing;

