package Game::Item::Weapon::Thrown;

use header;
use Moo;
use Game::Ability::Attribute::Physical;
use Game::Config;

no header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_THR';
use constant both_hands => 1;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->short_range;
use constant damage_deviation => 0.1;
use constant scaling => 'STT_STR:0.6;STT_AGI:0.4';
use constant base_damage_bonus => 0;

1;
