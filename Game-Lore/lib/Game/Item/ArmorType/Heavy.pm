package Game::Item::ArmorType::Heavy;

use header;
use Moo;

no header;

with 'Game::Item::ArmorType';

use constant lore_id => 'ART_HEA';
use constant endurance => 0.8;
use constant willpower => 0;

1;
