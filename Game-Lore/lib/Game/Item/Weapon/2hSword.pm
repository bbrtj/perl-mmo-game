package Game::Item::Weapon::2hSword;

use header;
use Moo;

no header;

extends 'Game::Item::Weapon::Sword';

use constant lore_id => 'WEA_2SWO';
use constant both_hands => 1;

1;
