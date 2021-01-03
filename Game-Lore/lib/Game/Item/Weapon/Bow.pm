package Game::Item::Weapon::Bow;

use header;
use Moo;
use Game::Ability::Attribute::Physical;
use Game::Config;

no header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_BOW';
use constant both_hands => 1;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->medium_range;
use constant damage_deviation => 0.15;
use constant scaling => 'STT_STR:0.1;STT_AGI:0.9';
use constant base_damage_bonus => 0;

1;
