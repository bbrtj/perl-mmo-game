package Game::Item::Weapon::Sword;

use My::Moose;
use Game::Ability::Attribute::Physical;
use Game::Config;

use header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_SWO';
use constant both_hands => 0;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->meele_range;
use constant damage_deviation => 0.1;
use constant scaling => 'STT_STR:0.7;STT_AGI:0.3';
use constant base_damage_bonus => 0;

