package Game::Repository::AbilityData;

use header;
use Moo;
use Game::Common::Container;

no header;

with 'Game::Repository::Role::Resource';

use constant ABILITY_LIST_FETCH_QUERY => <<'SQL';
	SELECT
		abi.id,
		abi.attribute_id as attribute,
		abi.passive,
		abi.cost,
		abi.cooldown,
		abi.range,
		abi.target_self,
		abi.target_ally,
		abi.target_foe,
		abi.target_ground,
		aef.type_id as effect_type,
		aef.attribute_id as effect_attribute,
		aef.effect_group,
		aef.value,
		aef.deviation
	FROM gd_abilities abi
	JOIN gd_ability_effects aef ON (abi.id = aef.ability_id)
SQL

sub save { ... }

sub load ($self)
{
	my $db = resolve('db');
	my $data = $db->query(ABILITY_LIST_FETCH_QUERY)->hashes;

	return $data->to_array;
}

1;
