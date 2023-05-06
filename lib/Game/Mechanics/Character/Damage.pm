package Game::Mechanics::Character::Damage;

use header;

sub deal_damage ($self, $attribute, $damage, @targets)
{
	my $log = DI->get('log');
	foreach my $target (@targets) {
		my $target_damage = $damage;

		# TODO dodge chance
		# TODO reduce damage (resistance buffs)
		# TODO amplify damage (amplification buffs)
		# TODO reduce damage (endurance / willpower)

		my $current_health = $target->variables->health - $target_damage;
		$target->variables->set_health($current_health);
		$log->debug(sprintf "%s now has %f health", $target->id, $current_health);
	}

	return;
}

