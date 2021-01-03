package Game::Item::Weapon::Staff;

use header;
use Moo;
use Game::Ability::Attribute::Physical;
use Game::Config;

no header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_STA';
use constant both_hands => 1;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->meele_range;
use constant damage_deviation => 0.05;
use constant scaling => 'STT_STR:0.3;STT_AGI:0.3';
use constant base_damage_bonus => 0.9;

1;
