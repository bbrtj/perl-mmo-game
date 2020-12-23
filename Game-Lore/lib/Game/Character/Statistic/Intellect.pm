package Game::Character::Statistic::Intellect;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_INT';
use constant focus_pp => 3;
use constant misc_crit_damage_pp => 0.5;

1;

__END__

=pod

Intellect is main stat for mage archetype
Bonuses:
- grants focus
- grants non-weapon critical strike damage

