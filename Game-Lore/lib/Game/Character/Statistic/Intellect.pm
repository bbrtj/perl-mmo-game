package Game::Character::Statistic::Intellect;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_INT';
use constant primary_bonus => 3;    # mana
use constant secondary_bonus => 0.5;    # misc crit dmg

1;

__END__

=pod

Intellect is main stat for mage archetype
Bonuses:
- grants mana
- grants non-weapon critical strike damage

