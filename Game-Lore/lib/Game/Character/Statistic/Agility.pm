package Game::Character::Statistic::Agility;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_AGI';
use constant dodge_pp => 0.5;
use constant crit_chance_pp => 0.5;

1;

__END__

=pod

Agility is main stat for rogue / hunter archetype
Bonuses:
- grants dodge rating
- grants critical strike chance

