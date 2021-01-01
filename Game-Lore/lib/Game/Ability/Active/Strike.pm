package Game::Ability::Active::Strike;

use header;
use Moo;
use Game::Ability::Attribute::Inherit;
use Game::Mechanics::Character::Damage;

no header;

with 'Game::Ability::Active';

use constant lore_id => 'ABI_STRIKE';
use constant attribute => Game::Ability::Attribute::Inherit->get;
use constant instant => 0;
use constant cost => 0;
use constant cooldown => 0;
use constant range => undef;
use constant aoe => undef;
use constant affect_ally => 0;
use constant affect_enemy => 1;
use constant affect_ground => 0;
use constant target_self => 0;
use constant target_ally => 0;
use constant target_enemy => 1;
use constant target_ground => 0;

sub effects ($self, $caster, $targets)
{
	# TODO weapon damage
	my $damage = 5;
	Game::Mechanics::Character::Damage->deal_damage($self->attribute, $damage, $targets);
}

1;
