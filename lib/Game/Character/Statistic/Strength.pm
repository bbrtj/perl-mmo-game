package Game::Character::Statistic::Strength;

use My::Moose;

use header;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_STR';
use constant primary_bonus => 1;    # endurance
use constant secondary_bonus => 0.5;    # critical strike damage

__END__

=pod

Strength is main stat for warrior archetype
Bonuses:
- grants endurance
- grants critical strike damage

