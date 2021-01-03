package Game::Item::Weapon::Dagger;

use header;
use Moo;
use Game::Ability::Attribute::Physical;
use Game::Config;

no header;

with 'Game::Item::Weapon';

use constant lore_id => 'WEA_DAG';
use constant both_hands => 0;
use constant attribute => Game::Ability::Attribute::Physical->get;
use constant range => Game::Config->meele_range;
use constant damage_deviation => 0.2;
use constant scaling => 'STT_STR:0.4;STT_AGI:0.6';
use constant base_damage_bonus => -0.05;

1;
