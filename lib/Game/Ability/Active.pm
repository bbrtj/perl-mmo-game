package Game::Ability::Active;

use My::Moose::Role;
use Utils;

use header;

with 'Game::Ability';

requires qw(
	attribute
	instant
	cost
	cooldown
	range
	aoe
	affect_ally
	affect_enemy
	affect_ground
	target_self
	target_ally
	target_enemy
	target_ground

	effects
);

sub _get
{
	return state $list =
		{map { $_->lore_id => $_->new } Utils->load_classes('Game::Ability::Active', 'Active/*.pm')};
}

