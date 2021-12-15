package Game::Item::Weapon::Crossbow;

use Moo;
use Game::Ability::Attribute::Physical;
use Game::Config;

use header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_CBOW';
use constant both_hands => 1;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->long_range;
use constant damage_deviation => 0.10;
use constant scaling => 'STT_AGI:0.5';
use constant base_damage_bonus => 0.12;

