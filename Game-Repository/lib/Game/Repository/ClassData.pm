package Game::Repository::ClassData;

use header;
use Moo;
use Game::Common::Container;

no header;

with 'Game::Repository::Role::Resource';

use constant CLASS_LIST_FETCH_QUERY => <<'SQL';
	SELECT
		cla.id,
		cla.playable,
		cla.base_health,
		cla.health_per_level,
		cla.base_health_regen,
		cla.health_per_level,
		cla.base_mana,
		cla.mana_per_level,
		cla.base_mana_regen,
		cla.mana_regen_per_level,
		cla.base_stats
		cla.stats_per_level,
		cab.ability_id as ability
	FROM gd_classes cla
	JOIN gd_class_abilities cab ON (cla.id = cab.class_id)
SQL

sub save { ... }

sub load ($self)
{
	my $db = resolve('db');
	my $data = $db->query(CLASS_LIST_FETCH_QUERY)->hashes;

	return $data->to_array;
}

1;
