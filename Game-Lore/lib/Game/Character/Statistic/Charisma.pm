package Game::Character::Statistic::Charisma;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_CHA';
use constant willpower_pp => 1;
use constant misc_crit_chance_pp => 0.5;

1;

__END__

=pod

Charisma is main stat for support archetype
Bonuses:
- grants willpower
- grants non-weapon critical strike chance

