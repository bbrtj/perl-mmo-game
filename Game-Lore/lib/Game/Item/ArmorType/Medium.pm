package Game::Item::ArmorType::Medium;

use header;
use Moo;

no header;

with 'Game::Item::ArmorType';

use constant lore_id => 'ART_MED';
use constant endurance => 0.6;
use constant willpower => 0;

1;
