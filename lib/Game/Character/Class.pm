package Game::Character::Class;

use Moo::Role;
use Game::Ability;

use header;

with 'Game::LoreElement';

requires qw(
	playable
	base_health
	health_per_level
	base_health_regen
	health_regen_per_level
	base_mana
	mana_per_level
	base_mana_regen
	mana_regen_per_level
	base_stats
	stats_per_level
);

sub _get
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Character::Class', 'Class/*.pm')};
}

