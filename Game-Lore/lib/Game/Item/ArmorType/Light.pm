package Game::Item::ArmorType::Light;

use header;
use Moo;

no header;

with 'Game::Item::ArmorType';

use constant lore_id => 'ART_LIG';
use constant endurance => 0.3;
use constant willpower => 0;

1;
