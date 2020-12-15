package Game::Ability::Parser;

use Mojo::Base -signatures;
use Game::Common::Container;
use Game::Ability::Compiled;
use Game::Ability::Compiled::Effect;

use constant ABILITY_LIST_FETCH_QUERY => <<SQL;
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

sub load()
{
	my $db = resolve('db');
	my $data = $db->query(ABILITY_LIST_FETCH_QUERY)->hashes;

	return $data;
}

sub find_effect_type($type)
{
	state $list = {
		map {
			$_->lore_id => $_->new
		} Game::Common->load_classes('Game::Ability::EffectType', 'EffectType/*.pm')
	};

	return $list->{$type};
}

sub find_attribute($type)
{
	state $list = {
		map {
			$_->lore_id => $_->new
		} Game::Common->load_classes('Game::Ability::Attribute', 'Attribute/*.pm')
	};

	return $list->{$type};
}

sub parse($class)
{
	my $data = load;
	my %loaded;

	for my $row ($data->each) {
		if (!exists $loaded{$row->{id}}) {
			$loaded{$row->{id}} = Game::Ability::Compiled->new(
				attribute => find_attribute($row->{attribute}),
				$row->%{qw(
					id
					passive cost
					cooldown range
					target_self
					target_ally
					target_foe
					target_ground
				)}
			);
		}

		$loaded{$row->{id}}->group($row->{effect_group})->add_effect(
			Game::Ability::Compiled::Effect->new(
				effect_type => find_effect_type($row->{effect_type}),
				attribute => find_attribute($row->{effect_attribute}),
				$row->%{qw(
					value deviation
				)}
			)
		);
	}

	return \%loaded;
}

1;
