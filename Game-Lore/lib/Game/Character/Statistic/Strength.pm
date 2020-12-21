package Game::Character::Statistic::Strength;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_STR';
use constant endurance_pp => 1;
use constant crit_damage_pp => 0.5;

1;

__END__

=pod

Strength is main stat for warrior archetype
Bonuses:
- grants endurance
- grants critical strike damage

