package Game::Character::Class::Bard;

use Moo;

use header;

with 'Game::Character::Class';

use constant lore_id => 'CLS_BAR';

use constant playable => 1;
use constant base_health => 110;
use constant health_per_level => 1;
use constant base_health_regen => 1;
use constant health_regen_per_level => 0.1;
use constant base_mana => 100;
use constant mana_per_level => 1;
use constant base_mana_regen => 1;
use constant mana_regen_per_level => 0.1;
use constant base_stats => 'STT_STR:10';
use constant stats_per_level => 'STT_STR:1';

