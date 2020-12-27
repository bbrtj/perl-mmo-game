package Game::Character::Statistic::Charisma;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_CHA';
use constant primary_bonus => 1;    # willpower
use constant secondary_bonus => 0.5;    # misc crit chance

1;

__END__

=pod

Charisma is main stat for support archetype
Bonuses:
- grants willpower
- grants non-weapon critical strike chance

