package Game::Item::ArmorType::Medium;

use My::Moose;

use header;

with 'Game::Item::ArmorType';

use constant lore_id => 'ART_MED';
use constant endurance => 0.6;
use constant willpower => 0;

