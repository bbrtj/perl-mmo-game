package Game::Mechanics::Character::Damage;

use header;

sub deal_damage ($self, $attributes, $damage, @targets)
{
	foreach my $target (@targets) {
		my $target_damage = $damage;

		# TODO reduce damage (resistance buffs)
		# TODO amplify damage (amplification buffs)
		# TODO reduce damage (endurance / willpower)

		$target->variables->set_health($target->variables->health - $target_damage);
	}

	return;
}

