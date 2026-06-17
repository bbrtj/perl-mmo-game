package Game::Mechanics::Character::Damage;

use Exporter qw(import);
use header;

our @EXPORT_OK = qw(
	deal_damage
);

sub deal_damage ($source_actor, $attributes, $damage, @targets)
{
	foreach my $target (@targets) {
		my $target_damage = $damage;

		# TODO reduce damage (resistance buffs)
		# TODO amplify damage (amplification buffs)
		# TODO reduce damage (endurance / willpower)

		$target->variables->set_health($target->variables->health - $target_damage);

		# TODO: call dibs on the npc
		if ($target->is_npc) {
			$target->npc->add_aggro($source_actor, $target_damage);
		}
	}

	return;
}

