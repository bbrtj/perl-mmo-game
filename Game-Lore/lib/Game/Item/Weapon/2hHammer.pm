package Game::Item::Weapon::2hHammer;

use header;
use Moo;

no header;

extends 'Game::Item::Weapon::Hammer';

use constant lore_id => 'WEA_2HAM';
use constant both_hands => 1;

1;
