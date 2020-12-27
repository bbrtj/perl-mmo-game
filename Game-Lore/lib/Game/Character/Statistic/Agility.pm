package Game::Character::Statistic::Agility;

use header;
use Moo;

with 'Game::Character::Statistic';

use constant lore_id => 'STT_AGI';
use constant primary_bonus => 0.5;    # critical strike chance
use constant secondary_bonus => 0.5;  # dodge rating

1;

__END__

=pod

Agility is main stat for rogue / hunter archetype
Bonuses:
- grants dodge rating
- grants critical strike chance

